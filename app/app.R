source('deps.R')
source('ga.R')
source('io.R')

plan(multiprocess)

options(shiny.autoreload=T)
options(shiny.host="0.0.0.0")
options(warn=-1)

flog.threshold(DEBUG)
OPTIMIZATION_LOG_FILE = Sys.getenv('OPTIMIZATION_LOG_FILE')
DEBUG_ENV = Sys.getenv('DEBUG') != ""
if (OPTIMIZATION_LOG_FILE != "") {
  flog.appender(appender.file('optimization.log'), name='optimization')
}
flog.debug('Logging setup complete')

NONE_SELECTED = '__NONE_SELECTED__'
NO_ASSIGNMENT = NONE_SELECTED

MIN_UTILIZATION = 0.9
MAX_UTILIZATION = 1.1

# Load data
units = readOGR('data/units.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
entities = readOGR('data/entities.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
weights = read_csv('data/weights.csv')
adjacency = read_csv('data/adjacency.csv', col_types ='cc')

# Add coordinates to adjacency data frame - just for debugging / visualization
row.names(units) = units$unit_id
adjacency = adjacency %>%
  inner_join(units %>% coordinates() %>% as.data.frame() %>% rename(from_long=V1, from_lat=V2) %>% mutate(from=rownames(.))) %>%
  inner_join(units %>% coordinates() %>% as.data.frame() %>% rename(to_long=V1, to_lat=V2) %>% mutate(to=rownames(.)))

assignment = units@data %>%
  select(unit_id) %>%
  left_join(read_csv('data/assignment.csv'), by='unit_id') %>%
  mutate(entity_id = ifelse(is.na(entity_id), NO_ASSIGNMENT, entity_id))

units = units %>% sp::merge(assignment)

rm(assignment)

bez = readOGR('data/RBS_OD_BEZ_2015_12.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE) %>% subset(BEZ == '07')

entity_ids = unique(entities$entity_id)
unit_ids = unique(units$unit_id)
optimizable_units = units %>% as.data.frame %>% filter(population > 0) %>% inner_join(weights, by='unit_id') %>% .$unit_id %>% unique
population_range = range(units$population, na.rm = T, finite = T)

flog.debug('Data loading complete')

### Helper functions

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# preferrably sample blocks closer to the school
# should be higher for smaller numbers
distance_sample_weights = unit_ids %>% map(function(unit_id) {
  idx = weights$unit_id == unit_id
  avg = as.double(weights[idx,]$avg)
  entity_ids = weights[idx,]$entity_id
  # TODO make function and divide exponent by optimization step
  weights = (1-(avg-min(avg))/max(avg-min(avg)))^3
  set_names(weights, entity_ids)
}) %>% set_names(unit_ids)

distance_sample_probs = function(unit_id, heuristic_exponent=3) {
  weights = distance_sample_weights[[unit_id]]
  pot_weights = weights^heuristic_exponent
  pot_weights/sum(pot_weights)
}

unit_index = set_names(1:length(units$unit_id), units$unit_id)

entities$selected = F
# Remark: There is no entities$updated because they have to be redrawn anyway
entities$highlighted = T # highlight when selected
entities$hovered = F
units$selected = F # selected units can be reassigned, locked, etc
units$highlighted = T # highlight units when assigned entity is selected
units$locked = F
units$updated = T # only updated unites need to be redrawn (true for initial render)
units$hovered = F

# Scales
# cfac = colorFactor(rainbow(length(entity_ids)), levels=sample(entity_ids))
colors = brewer.pal(8, 'Set1') # show_col(brewer.pal(9,'Set1'))
valid_colors = colors[2:8]
warning_color =  colors[1]
entity_ids_color = c(
  "07G16", "07G02", "07G01", "07G21", "07G14", "07G07", "07G15", "07G03", "07G13", "07G12", "07G10", "07G17", "07G06", "07G18",
  "07G19", "07G24", "07G05", "07G22", "07G23", "07G25", "07G20", "07G36", "07G27", "07G37", "07G35", "07G30", "07G28", "07G32",
  "07G29", "07G34", "07G26", "07G31"
)
palette = rep(valid_colors, length(entity_ids_color)/length(valid_colors)+1)[1:length(entity_ids_color)]
cfac = colorFactor(palette, levels=entity_ids_color)
entity_colors = function(entity_id, desaturate = FALSE) {
  color = cfac(entity_id)
  ifelse(desaturate, desat(color, 0.3), color)
}

desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}

# Colors for Report

color_vec = c(palette, warning_color)
names(color_vec) = c(entity_ids_color, NO_ASSIGNMENT)

### UI
ui <- fillPage(
  shinyStore::initStore("store", "shinyStore-ize1"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel="shortcut icon", href="http://idalab.de/favicon.ico"),
    tags$title(HTML("idalab - intelligent zoning engine")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML(
      do.call(paste0, map2(entity_ids_color, cfac(entity_ids_color), ~ paste0(".entity-color-", .x, " {color:", .y, " !important;}.entity-bg-", .x, " {background-color:", .y, "!important;}\n")))
      ))
  ),
  # stripes pattern
  div(
    id='svg-patterns',
    HTML('<svg height="5" width="5" xmlns="http://www.w3.org/2000/svg" version="1.1"> <defs> <pattern id="locked-pattern" patternUnits="userSpaceOnUse" width="5" height="5"> <image xlink:href="data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1IiBoZWlnaHQ9IjUiPgo8cGF0aCBkPSJNMCA1TDUgMFpNNiA0TDQgNlpNLTEgMUwxIC0xWiIgc3Ryb2tlPSIjMDAwIiBzdHJva2Utd2lkdGg9IjEiPjwvcGF0aD4KPC9zdmc+" x="0" y="0" width="5" height="5"> </image> </pattern> </defs> </svg>'),
    HTML('<svg height="10" width="10" xmlns="http://www.w3.org/2000/svg" version="1.1"> <defs> <pattern id="diagonal-stripe-1" patternUnits="userSpaceOnUse" width="10" height="10"> <image xlink:href="data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxMCcgaGVpZ2h0PScxMCc+CiAgPHJlY3Qgd2lkdGg9JzEwJyBoZWlnaHQ9JzEwJyBmaWxsPSd3aGl0ZScvPgogIDxwYXRoIGQ9J00tMSwxIGwyLC0yCiAgICAgICAgICAgTTAsMTAgbDEwLC0xMAogICAgICAgICAgIE05LDExIGwyLC0yJyBzdHJva2U9J2JsYWNrJyBzdHJva2Utd2lkdGg9JzEnLz4KPC9zdmc+Cg==" x="0" y="0" width="10" height="10"> </image> </pattern> </defs> </svg>'),
    HTML('<svg height="8" width="8" xmlns="http://www.w3.org/2000/svg" version="1.1"> <defs> <pattern id="crosshatch" patternUnits="userSpaceOnUse" width="8" height="8"> <image xlink:href="data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPSc4JyBoZWlnaHQ9JzgnPgogIDxyZWN0IHdpZHRoPSc4JyBoZWlnaHQ9JzgnIGZpbGw9JyNmZmYnLz4KICA8cGF0aCBkPSdNMCAwTDggOFpNOCAwTDAgOFonIHN0cm9rZS13aWR0aD0nMC41JyBzdHJva2U9JyNhYWEnLz4KPC9zdmc+Cg==" x="0" y="0" width="8" height="8"> </image> </pattern> </defs> </svg>
')
  ),
  fillRow(
    div(
      id='map-panel',
      leafletOutput("map", width="100%", height='100%'),
      div(id="map-controls",
          checkboxInput("show_utilization", "Auslastung anzeigen", value = TRUE),
          checkboxInput("show_population", "Population anzeigen", value = FALSE)
          ),
      tabsetPanel(type="tabs", id="tabs",
                  tabPanel("Details", div(id='detail',
                      fillRow(
                        div(id='detail--entity',
                            h5('Schule:'),
                            h4(id='detail--entity--selected-school', uiOutput('selected_entity')),
                            div(id='detail--entity--controls',
                                actionButton('deselect_entity', '', icon=icon('remove'))
                            ),
                            tableOutput('selected_entity_table')
                        ),
                        div(id='detail--units',
                            h5('Blöcke:'),
                            h4(id='detail--units--selected-units', uiOutput('selected_units')),
                            div(id='detail--units--controls',
                                actionButton('deselect_units', '', icon=icon('remove')),
                                actionButton('assign_units', '', icon=icon('link')),
                                actionButton('deassign_units', '', icon=icon('unlink')),
                                actionButton('lock_units', '', icon=icon('lock')),
                                actionButton('unlock_units', '', icon=icon('unlock'))
                            ),
                            tableOutput('selected_units_table')
                        )
                      )
                      # TODO move buttons into UI outputs
                  )),
                  tabPanel("Import/Export", div(id='io',
                      downloadButton('report', 'Report'),
                      downloadButton('serveGeoJSON', 'GeoJSON'),
                      downloadButton('serveAssignment', 'Zuordnung Herunterladen'),
                      fileInput('readAssignment', 'Zuordnung Hochladen',
                                accept = c('text/csv',
                                           'text/comma-separated-values',
                                           'text/plain',
                                           '.csv')
                                ),
                      actionButton('reset_assignment', 'Reset', icon=icon('fast-backward'))
                  )),
                  tabPanel("Optimierung", div(id='optimize',
                      uiOutput('optimize_button', inline = TRUE),
                      plotOutput('fitness', height = '120px')
                  )),
                  tabPanel("Über das Projekt", div(id='about',
                    includeMarkdown("about.md")
                  ))
      )
    ),
    div(
      id='table-panel',
      DT::dataTableOutput("table")
    )
  )
)

### Server
server <- function(input, output, session) {

  ### Reactive Values

  r = reactiveValues(
    units=units, entities=entities,
    assignment_rev=0, # assignment revision - indicator that a block was reassigned manually
    selected_entity=NONE_SELECTED,
    previous_selected_entity=NONE_SELECTED,
    selected_entity_index=NULL, # only one can be selected
    show_utilization = T,
    show_population = F,
    running_optimization=FALSE,
    optimization_step=0,
    previous_mouseover=NONE_SELECTED)
  
  # variables for R optimization implementation
  ga = reactiveValues(
    population_future=future(NULL),
    population=NULL
    )

  ### Interaction

  observeEvent(input$optimize, {
    flog.debug('Optimize button pressed')
    if (r$running_optimization) {
      stop_optimization()
      r$running_optimization = FALSE
    } else {
      r$running_optimization = TRUE
      start_optimization(r$units)
    }
  })
  
  observeEvent(input$assign_units, {
    flog.debug('Assign button pressed')
    r$units[r$units$selected, 'entity_id'] = r$selected_entity
    r$units[r$units$selected, 'highlighted'] = T
    r$units[r$units$selected, 'updated'] = T
    r$assignment_rev = r$assignment_rev + 1
    reset_optimization(r$units)
  })
  
  observeEvent(input$deassign_units, {
    flog.debug('Deassign button pressed')
    r$units[r$units$selected, 'entity_id'] = NONE_SELECTED
    r$units[r$units$selected, 'highlighted'] = T # FIXME wat?
    r$units[r$units$selected, 'updated'] = T
    r$assignment_rev = r$assignment_rev + 1
    reset_optimization(r$units)
  })
  
  observeEvent(input$deselect_units, {
    flog.debug('Deselect button pressed')
    r$units[r$units$selected, 'updated'] = T
    r$units$selected = F
    r$assignment_rev = r$assignment_rev + 1
  })
  
  observeEvent(input$lock_units, {
    flog.debug('Lock button pressed')
    r$units[r$units$selected, 'locked'] = T
    r$units[r$units$selected, 'updated'] = T
    r$assignment_rev = r$assignment_rev + 1
    reset_optimization(r$units)
  })
  
  observeEvent(input$unlock_units, {
    flog.debug('Unlock button pressed')
    r$units[r$units$selected, 'locked'] = F
    r$units[r$units$selected, 'updated'] = T
    r$assignment_rev = r$assignment_rev + 1
    reset_optimization(r$units)
  })
  
  observeEvent(input$deselect_entity, {
    flog.debug('Deselect button pressed')
    r$selected_entity = NONE_SELECTED
    r$selected_entity_index = NULL
  }) 
  
  ### Map controls
  
  observe({
    shinyjs::toggleClass("map-panel", "show-utilization", input$show_utilization)
  })

  observeEvent(input$show_population, {
    r$units$updated = T
  })
  
  ### selection status
  
  observe({
    shinyjs::toggleClass('map-panel', 'entity-selected', r$selected_entity != NONE_SELECTED) 
  })
  
  observe({
    shinyjs::toggleClass('map-panel', 'units-selected', sum(r$units$selected) > 0) 
  })
  
  ### Localstorage of assignment
  
  observeEvent(input$reset_assignment, {
    shinyStore::updateStore(session, "assignment", NULL)
    r$units = units
    r$assignment_rev = r$assignment_rev + 1
  })
  
  observeEvent(r$assignment_rev, {
    if (r$assignment_rev == 0) {
      try(isolate({
        assignment = input$store$assignment %>% charToRaw %>% unserialize
        stopifnot(c('unit_id', 'entity_id', 'locked') %in% colnames(assignment))
        assignemnt = r$units %>% as.data.frame() %>% select(unit_id) %>% left_join(assignment)
        r$units$entity_id = ifelse(is.na(assignment$entity_id), r$units$entity_id, assignment$entity_id)
        r$units$locked = ifelse(is.na(assignment$locked), r$units$locked, assignment$locked)
      }))
    }
    shinyStore::updateStore(session, "assignment", r$units %>% as.data.frame() %>% select(unit_id, entity_id, locked) %>% serialize(NULL, ascii=T) %>% rawToChar)
  })
  
  ### load assignment

  observeEvent(input$readAssignment, {
    flog.debug('upload button pressed')
    num_warn = length(warnings())
    upload = read_csv(input$readAssignment$datapath)
    num_warn = length(warnings()) - num_warn
    validate(
      need(num_warn == 0,
           sprintf("Es gab %d Problem(e) mit der Datei.", num_warn))
      # need(),  # all unit_ids must be valid
      # need()  # all entity_ids must be valid
    )
    prev_entities = r$units$entity_id
    new_entities = r$units %>%
      as.data.frame() %>%
      select(unit_id) %>%
      left_join(upload, by="unit_id") %>% .$entity_id
    r$units$entity_id = ifelse(is.na(new_entities), NONE_SELECTED, new_entities)
    r$units$updated = TRUE
    r$assignment_rev = r$assignment_rev + 1
  })

  # unit mouseover -> highlight the shape
  observe({
    return()
    req(input$map_shape_mouseover$id)
    req(input$map_shape_mouseover$group)
    flog.debug('mouseover event')
    if (input$map_shape_mouseover$id != isolate(r$previous_mouseover)) {
      # run this again and only set selected block if we're over the same shape after 100ms
      r$previous_mouseover = input$map_shape_mouseover$id
      invalidateLater(100, session)
    } else if (input$map_shape_mouseover$group == 'units') {
      # hover the respective block
      hovered_unit = sub('unit_', '', input$map_shape_mouseover$id)
      flog.debug('mouseover %s', hovered_unit)
      isolate({
        r$units[r$units$hovered, 'updated'] = T
        r$units$hovered = F
        r$units[unit_index[[hovered_unit]], 'hovered'] = T
        r$units[unit_index[[hovered_unit]], 'updated'] = T
      })
    }
  })

  observeEvent(input$map_shape_mouseout, {
    return()
    req(input$map_shape_mouseout$id)
    req(input$map_shape_mouseout$group)
    flog.debug('mouseout event')
    if (input$map_shape_mouseout$group == 'units') {
      # unhover the respective block
      hovered_unit = sub('unit_', '', input$map_shape_mouseout$id)
      flog.debug('mouseout %s', hovered_unit)
      r$units[unit_index[[hovered_unit]], 'hovered'] = F
      r$units[unit_index[[hovered_unit]], 'updated'] = T
    }
  })

  # click on markers -> select entity and highlight units
  # only changes r$selected_entity and r$selected_entity_index
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$id)
    req(input$map_marker_click$group)
    flog.debug('marker %s clicked', input$map_marker_click$id)
    isolate({
      if (input$map_marker_click$group == "entities") {
        clicked_marker = sub('entity_', '', input$map_marker_click$id)
        r$previous_selected_entity = r$selected_entity
        r$selected_entity = ifelse(clicked_marker == r$selected_entity, NONE_SELECTED, clicked_marker)
        index = entities$entity_id %>% detect_index(~ .x == r$selected_entity)
        if (is.null(index) | index == 0) r$selected_entity_index = NULL else r$selected_entity_index = index
      }
    })
    # this will trigger an update of the r$selected_entity watcher where $updated and $highlighted are calculated
  })

  # click on table row
  # only changes r$selected_entity and r$selected_entity_index
  observe({
    row = input$table_rows_selected
    flog.debug('row %s clicked', row)
    isolate({
      r$selected_entity_index = row
      r$selected_entity = ifelse(is.null(row), NONE_SELECTED, entities$entity_id[row])
    })
    # this will trigger an update of the r$selected_entity watcher where $updated and $highlighted are calculated
  })

  # click on shapes -> update the selected status
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    req(input$map_shape_click$group)
    flog.debug('shape %s clicked', input$map_shape_click$id)
    if (input$map_shape_click$group == 'units') {
      # a unit was clicked
      clicked_unit = sub('unit_', '', input$map_shape_click$id)
      # flip clicked unit
      r$units[r$units$unit_id == clicked_unit, 'selected'] = !r$units[r$units$unit_id == clicked_unit,]$selected
      r$units[r$units$unit_id == clicked_unit, 'updated'] = T
    }
  })

  # r$selected_entity watcher: entity selection changed - update blocks
  observe({
    selected_entity = r$selected_entity
    
    isolate({
      flog.debug("selected entity changed to %s from %s", r$selected_entity, r$previous_selected_entity)

      previously_highlighted = rep(r$entities$highlighted)
      r$entities$highlighted = r$entities$entity_id == selected_entity | selected_entity == NONE_SELECTED

      previously_highlighted = rep(r$units$highlighted)
      r$units$highlighted = r$units$entity_id == selected_entity | selected_entity == NONE_SELECTED
      r$units$updated = r$units$updated | (r$units$highlighted != previously_highlighted)
    })
  })

  ### Update the UI

  updateMap = function(map, units, show_population = FALSE) {
    flog.debug('updateMap')
    flog.debug('nrow(units) %s', nrow(units))
    if (nrow(units) > 0) {
      flog.debug('redrawing units')
      replaceNA = function(x, replace) ifelse(is.na(x), replace, x)
      map = map %>%
        # redraw updated selected units
        addPolygons(
          data=units, group='units', layerId=~paste0('unit_', unit_id),
          stroke = F,
          fillOpacity = ~ replaceNA(
            ifelse(rep(show_population, nrow(units)),
                   scales::rescale(population, to=c(0.1, 1), from=population_range)
                   , 1),
            1),
          smoothFactor = 0.2,
          color = ~ ifelse( # TODO factor into function
              entity_id == NONE_SELECTED & unit_id %in% optimizable_units,
              ifelse(!highlighted, desat(warning_color, 0.3), warning_color),
              entity_colors(entity_id, desaturate = !highlighted)
              )
        ) %>%
        # locked
        addPolygons(
          data=units, color=NULL, fillOpacity=NULL,
          group='locked_units', layerId=~paste0('locked_unit_', unit_id),
          options=pathOptions(pointerEvents='none', className=~ifelse(locked, 'locked', 'unlocked'))
        ) %>%
        # draw boundaries of selected units
        addPolylines(
          data=units, color=~ifelse(selected, 'red', 'transparent'), weight=4,
          group='selected_units', layerId=~paste0('selected_unit_', unit_id),
          options=pathOptions(pointerEvents='none', className='selected_units')
        )
    }
    # always draw entities on top
    capacity_warning = r$entities %>% as.data.frame() %>%
      left_join(r$units %>% as.data.frame() %>% group_by(entity_id) %>% summarise(pop=sum(population))) %>%
      mutate(utilization=pop/capacity, warning=ifelse(utilization < MIN_UTILIZATION, 'under-capacity', ifelse(utilization > MAX_UTILIZATION, 'over-capacity', ''))) %>% .$warning
    warning_radius = r$entities %>% as.data.frame() %>%
      left_join(r$units %>% as.data.frame() %>% group_by(entity_id) %>% summarise(pop=sum(population))) %>%
      mutate(utilization=pop/capacity,
             utilization_diff=abs(utilization-1),
             warning_radius=pmax(7, pmin(12, scales::rescale(utilization_diff, c(7, 12), c(0.1, 1))))) %>%
      .$warning_radius
    map = map %>%
      addCircleMarkers(
        data=r$entities, group='entities-warning', layerId=~paste0('entity_warning_', entity_id),
        options=pathOptions(pointerEvents='none', className=~paste('capacity-indicator', capacity_warning)),
        fillOpacity=NULL, color=NULL, opacity=NULL, fillColor=NULL,
        weight=2, radius=~warning_radius
      ) %>%
      addCircleMarkers(
        data=r$entities, group='entities', layerId=~paste0('entity_', entity_id),
        label = ~entity_id,
        options=pathOptions(className=~paste('entity', ifelse(entity_id == r$selected_entity, 'selected', ''))),
        fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, fillColor= ~entity_colors(entity_id, desaturate = !highlighted)
      )
    return(map)
  }
  
  # Initial map render
  output$map <- renderLeaflet({
    isolate({
      m <- leaflet() %>%
        addProviderTiles("Stamen.Toner", option=providerTileOptions(opacity=0.2)) %>%  # Add default OpenStreetMap map tiles
        addPolylines(color='black', weight=4, opacity=1, data=bez) %>%
        updateMap(r$units, input$show_population)
      r$units$updated = F
      flog.debug('Map initialized')
      m      
    })
  })

  # incremental map update
  observe({
    updated_units = r$units[r$units$updated,]
    show_population = input$show_population
    isolate({
      leafletProxy("map") %>% updateMap(updated_units, show_population)
    })
    r$units$updated = F
    flog.debug('Map updated')
  })

  ### optimization
  
  ## High level functions
  #
  # - start_optimization(new_assignment: data.frame(unit_id, entity_id, locked))
  #   sends the initial assignment or updated assignments after user interaction
  # - stop_optimization() don't run the optimization any more
  # - is_optim_solution_updated() check if there is a new solution
  # - get_optim_solution(): list(solution=data.frame(unit_id, entity_id, locked), score=Numeric) # FIXME locked needed?
  #   fetches the current best solution

  start_optimization = function(assignment) {
    # TODO talk to python
    flog.info('(Re-)Starting optimization')
    r$optimization_step = 0
    # ga$population_future = future(NULL) # FIXME this causes a lot of futures to be calculated in parallel
    forget(mem_fitness_f)
    reset_ga_population(assignment)
    # actual optimization is started in main optimization loop # TODO should happen here for python
  }
  
  reset_optimization = function(assignment) {
    if (r$running_optimization) {
      start_optimization(assignment)
    }
  }
  
  stop_optimization = function() {
    # TODO talk to python
  }
  
  is_optim_solution_updated = function() {
    # TODO talk to python
    if (resolved(ga$population_future)) {
      future_population = value(ga$population_future)
      !is.null(future_population)
    } else FALSE
  }
  
  get_optim_solution = function() {
    # TODO talk to python
    future_population = value(ga$population_future)
    ga$population = future_population
    fittest = ga$population[[1]]
    score = mem_fitness_f(fittest)
    list(solution=fittest, score=score)
  }
  
  # main optimization loop
  observe({
    if (r$running_optimization) {
      isolate({
        # Start initial optimization step # TODO this should happen in Python automatically
        if (resolved(ga$population_future) & is.null(value(ga$population_future))) {
          r$optimization_step = r$optimization_step + 1
          next_optimization_step(r$optimization_step)
        }
        
        # get updated solution
        if (is_optim_solution_updated()) {
          flog.info('Finished optimization step %s', r$optimization_step)
          r$optimization_step = r$optimization_step + 1
          
          solution = get_optim_solution()

          r$fittest_fitness = c(r$fittest_fitness, solution$score)
          
          # update relevant values for ui updates
          prev_entities = r$units$entity_id
          new_entities = r$units %>% as.data.frame() %>% select(unit_id) %>%
            left_join(solution$solution, by="unit_id") %>% .$entity_id
          r$units$entity_id = ifelse(is.na(new_entities), prev_entities, new_entities)
          r$units$updated = r$units$updated | r$units$entity_id != prev_entities
            
          r$assignment_rev = r$assignment_rev + 1
          
          # Start new optimization step # TODO this should happen in Python automatically
          next_optimization_step(r$optimization_step)
        } # otherwise just skip and do nothing in this iteration
        
      })
      invalidateLater(100, session)
    }
  })

  output$fitness = renderPlot({
    ggplot() +
      geom_line(aes(x=seq_along(r$fittest_fitness), y=r$fittest_fitness)) +
      geom_line(aes(x=seq_along(diff(r$fittest_fitness))+1, y=diff(r$fittest_fitness)*(-1)), linetype=2) +
      expand_limits(y=0) + labs(x = 'Optimierungsschritt', y = 'Kostenfunktion')
  })
  
  if (DEBUG_ENV) {
    # after each assignment change plot the best solutions fitness
    observeEvent(r$assignment_rev, {
      current = units %>% as.data.frame() %>%
        filter(unit_id %in% optimizable_units) %>%
        select(unit_id) %>%
        left_join(r$units %>% as.data.frame() %>% select(unit_id, entity_id, locked), by='unit_id') %>%
        mutate(entity_id=ifelse(entity_id == NO_ASSIGNMENT, sample(entity_ids, length(is.na(entity_id)), replace = T), entity_id))
      
      mem_fitness_f(current, verbose=T)
    })
  }

  ### genetic algorithm
  
  ## R implementation of GA # TODO remove
  
  reset_ga_population = function(assignment) {
    # select only blocks with stats and assign random schools for unassigned blocks
    current = units %>% as.data.frame() %>%
      filter(unit_id %in% optimizable_units) %>%
      select(unit_id) %>%
      left_join(assignment %>% as.data.frame() %>% select(unit_id, entity_id, locked), by='unit_id') %>%
      mutate(entity_id=ifelse(entity_id == NO_ASSIGNMENT, sample(entity_ids, length(is.na(entity_id)), replace = T), entity_id))
    
    ga$population = list(current)
  }
  
  next_optimization_step = function(step) {
    heuristic_exponent = 20/step^(1) # TODO = 1/2?
    mutation_fraction = max(0.001, 1-(step-1)/3)
    flog.info('Running optimization step %s with mutation_fraction %s and heuristic_exponent %s', step, mutation_fraction, heuristic_exponent)
    
    ga$population_future = future({
      ga_select(
        ga_breed(ga$population,
                 ga_mutate=mutation_f,
                 ga_crossover=crossover_f,
                 fitness_f=mem_fitness_f,
                 mutation_fraction=mutation_fraction,
                 num_pairs = 50,
                 num_mutants = 50,
                 heuristic_exponent = heuristic_exponent),
        mem_fitness_f,
        max_population = 50)
    })  
  }
  
  # randomly reassign a number of schools
  mutation_f = function(individual, fraction=0.05, heuristic_exponent=3) {
    if (fraction == 0) return(individual)
    #cost = rep(1, nrow(individual))
    #cost = 1-cost
    locked_idx = (1:nrow(individual))[individual$locked]
    mutatable_idx = setdiff(1:nrow(individual), locked_idx)
    num_mutations = min(ceiling(fraction*nrow(individual)), length(mutatable_idx))
    # preferrably select high costs blocks to mutate # FIXME does this hurt dense areas?
    prob = individual[mutatable_idx,] %>% inner_join(weights, by=c('unit_id', 'entity_id')) %>%
      transmute(avg=avg, cost=(avg/max(avg))^heuristic_exponent, prob=cost/sum(cost)) %>% .$prob
    mutation_idx = sample(mutatable_idx, num_mutations, prob = prob)
    mutation_units = as.character(individual[mutation_idx, 'unit_id'])
    # TODO flip pairs instead of random assignment
    # preferrably select schools closer to the block
    mutations = mutation_units %>% map(
      ~ sample(names(distance_sample_probs(.x, heuristic_exponent)), 1, replace=T, prob=distance_sample_probs(.x, heuristic_exponent))
      )
    individual[mutation_idx, 'entity_id'] = unlist(mutations)
    individual
  }

  # randomly mix assignments from two individuals into two new ones
  # TODO only where they are different
  crossover_f = function(a, b) {
    # only select from genes that are different
    difference = a$entity_id != b$entity_id
    if (sum(difference)==0) return(list(a, b))
    child_a = a[,]
    child_b = b[,]
    fraction = runif(1)
    # locked should always be the same for a and b # FIXME can be more efficient!
    locked_idx = (1:nrow(a))[a$locked]
    mutatable_idx = setdiff((1:nrow(a))[difference], locked_idx)
    num_genes = min(ceiling(fraction*sum(difference)), length(mutatable_idx))
    swap_idx = sample(mutatable_idx, num_genes)
    child_a[swap_idx, 'entity_id'] = b[swap_idx, 'entity_id']
    child_b[swap_idx, 'entity_id'] = a[swap_idx, 'entity_id']
    list(child_a, child_b)
  }

  fitness_f = function(individual, verbose=FALSE) {
    OVER_CAPACITY_PENALTY = 1
    UNDER_CAPACITY_PENALTY = 1
    DIST_WEIGHT = 1/1000^2 # 1000m means penalty of 1
    OVER_CAPACITY_WEIGHT = 1/20/10 # 20 over capacity means penalty of 0.1
    UNDER_CAPACITY_WEIGHT = 1/20/10 # 20 under capacity means penalty of 0.1
    
    # TODO coherence cost as number of connected components
    filtered_edges = adjacency %>%
      inner_join(individual, by=c('from'='unit_id'), copy = T) %>%
      inner_join(individual %>% select(to_unit_id=unit_id, to_entity_id=entity_id), by=c('to'='to_unit_id'), copy = T) %>%
      filter(entity_id == to_entity_id)
    
    # browser() here to debug connected components with following plot:
    # ggplot() +
    #  geom_polygon(aes(x=long, y=lat, group=group), fill='gray', data=broom::tidy(units, region='unit_id')) +
    #  geom_segment(aes(x=from_long, y=from_lat, xend=to_long, yend=to_lat), size=0.1, color='black', data=filtered_edges) +
    #  theme_nothing() + coord_map()
    
    optimum_connected_components = length(entity_ids)
    connected_components = igraph::count_components(igraph::graph_from_data_frame(filtered_edges, directed = F, vertices = optimizable_units))
    coherence_cost = connected_components/optimum_connected_components
    
    individual %>% inner_join(units %>% as.data.frame %>% select(unit_id, population), by='unit_id') %>% # FIXME faster?
      inner_join(weights, by=c('unit_id', 'entity_id')) %>%
      group_by(entity_id) %>%
      summarise(
        avg=sum(avg*population)/sum(population), # population weighted mean
        max=max(max), # per catchment area look at maximum distance
        population=sum(population)
        ) %>%
      inner_join(entities %>% as.data.frame %>% select(entity_id, capacity), by='entity_id') %>% # FIXME faster?
      rowwise() %>% mutate(over_capacity=max(1, population-capacity), under_capacity=max(1, capacity-population)) %>% ungroup %>%
      mutate(over_capacity_penalty=(over_capacity*OVER_CAPACITY_PENALTY)^2, under_capacity_penalty=(under_capacity*UNDER_CAPACITY_PENALTY)^2) %>%
      summarise(
        avg=mean(avg^2), # squared average distance
        max=mean(max^2), # squared average maximum distance
        over_capacity_penalty=mean(over_capacity_penalty),
        under_capacity_penalty=mean(under_capacity_penalty)
        ) %>%
      mutate(
        avg = DIST_WEIGHT*avg,
        max = DIST_WEIGHT*max,
        over_capacity_penalty = OVER_CAPACITY_WEIGHT*over_capacity_penalty,
        under_capacity_penalty = UNDER_CAPACITY_WEIGHT*under_capacity_penalty
        ) %>%
      (function(x) {
        if (verbose) {
          flog.debug('*** error components', name='optimization')
          flog.debug('Avg distance error: %s', x$avg, name='optimization')
          flog.debug('Max distance error: %s', x$max, name='optimization')
          flog.debug('Over capacity error: %s', x$over_capacity_penalty, name='optimization')
          flog.debug('Under capacity error: %s', x$under_capacity_penalty, name='optimization')
          flog.debug('Coherence cost: %s (%s connected components/%s schools)', coherence_cost, connected_components, optimum_connected_components, name='optimization')
          flog.debug('***', name='optimization')
        }
        x
      }) %>%
      mutate(fitness = max + avg + over_capacity_penalty + under_capacity_penalty + coherence_cost) %>% .$fitness
  }

  mem_fitness_f = memoise(fitness_f)
  
  ### /genetic algorithm

  ### table

  reactive_table_data = reactive({
    rev = r$assignment_rev # recalculate if assignment_rev is altered
    isolate(r$units) %>% as_data_frame() %>%
      filter(entity_id != NO_ASSIGNMENT) %>%
      left_join(weights, by=c('unit_id', 'entity_id')) %>%
      group_by(entity_id) %>%
      summarise(
        num_units=n(),
        min_dist=min(min, na.rm=T),
        avg_dist=sum(population*avg, na.rm=T)/sum(population, na.rm=T), # population weighted mean
        max_dist=max(max, na.rm=T),
        pop=sum(population, na.rm=T),
        sgbIIu65=sum(population*sgbIIu65, na.rm=T)/sum(population, na.rm=T) # FIXME population is only kids...
      ) %>%
      left_join(entities %>% as.data.frame %>% select(entity_id, BZR, capacity), by='entity_id') %>%
      mutate(
        utilization=pop/capacity
      )
  })
  
  renamed_table_data = reactive({
      d = reactive_table_data() %>%
        select(
          Schule=entity_id,
          Kapazität=capacity,
          Kinder=pop,
          Auslastung=utilization,
          `SGBII(u.65)`=sgbIIu65,
          `Weg (Ø)`=avg_dist,
          `Weg (max)`=max_dist,
          BZR=BZR
        )
  })

  ### Outputs


  rowCallback = DT::JS("function(row, data) {",
  "if (data[4] < ", MIN_UTILIZATION, ") {",
  "
    $(row).addClass('under-capacity').removeClass('over-capacity').removeClass('capacity-ok');
  } else if (data[4] > ", MAX_UTILIZATION, ") {
    $(row).addClass('over-capacity').removeClass('under-capacity').removeClass('capacity-ok');
  } else {
    $(row).addClass('capacity-ok').removeClass('under-capacity').removeClass('over-capacity');
  }

  $('td:eq(4)', row).prepend('<i class=\"glyphicon glyphicon-ok\"></i><i class=\"fa glyphicon glyphicon-remove\"></i> ');

  $('td:eq(1)', row).append('<span class=\"entity-color-indicator entity-bg-'+data[1]+'\"></span>');
  $('td:eq(0)', row).prepend('<span class=\"entity-color-indicator entity-bg-'+data[1]+'\"></span>');
  ",
  "}")

  # initial table render (see later observe for update)
  output$table = DT::renderDataTable({
    data = isolate(renamed_table_data())
    
    data %>%
      datatable(
        options=list(processing = F, paging = F, searching = F, rowCallback = rowCallback, columnDefs=list(list(targets=c(2,3,5,6,7), class="dt-right"))),
        selection=list(mode = 'single', selected = isolate(r$selected_school_index), target = 'row')
      ) %>%
      formatPercentage(c('Auslastung', 'SGBII(u.65)'), digits = 2) %>%
      formatRound(c(
        'Kinder',
        'Weg (max)',
        'Weg (Ø)')) %>%
      formatStyle(
        'Weg (Ø)',
        background = styleColorBar(data$`Weg (Ø)`, 'lightskyblue'),
        backgroundSize = '92% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Weg (max)',
        background = styleColorBar(data$`Weg (max)`, 'coral'),
        backgroundSize = '92% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )

  }, server = T)

  tableProxy = DT::dataTableProxy('table')

  observe({
    tableProxy %>% replaceData(renamed_table_data(), clearSelection='none')
  })

  observe({
    flog.debug('r$selected_entity_index changed to %s', r$selected_entity_index)
    currently_selected = isolate(input$table_rows_selected)
    should_be_selected = r$selected_entity_index
    # This comparison with the actual value is necessary because otherwise it triggers an endless loop
    if (!is.null(currently_selected) && is.null(should_be_selected)) {
      tableProxy %>% selectRows(NULL)
    } else if (is.null(currently_selected) && !is.null(should_be_selected)) {
      tableProxy %>% selectRows(should_be_selected)
    } else if (!is.null(currently_selected) && !is.null(should_be_selected) && currently_selected != should_be_selected) {
      tableProxy %>% selectRows(should_be_selected)
    }
  })

  # Maybe via row callbacks? https://rstudio.github.io/DT/options.html
  
  ### Variables for detail views
  
  warnIfGt = function(num, thres, s) {
    ifelse(num > thres, paste0('<div class="warning">',s,'</div>'), s)
  }

  output$selected_entity = renderUI({
    if (r$selected_entity != NONE_SELECTED) {
      entity = entities@data %>% filter(entity_id == r$selected_entity)
      HTML(paste0(r$selected_entity, '<span class="entity-color-indicator entity-bg-', r$selected_entity, '"></span>', entity$SCHULNAME))
    } else {
      HTML('Keine ausgewählt')
    }
  })
  
  output$selected_entity_table = renderTable({
    if (r$selected_entity != NONE_SELECTED) {
      d = reactive_table_data()[reactive_table_data()$entity_id == r$selected_entity,] %>%
        transmute(
          `Kapazität` = capacity,
          Kinder = warnIfGt(pop, capacity, formatC(pop, digits=2, format='f')),
          Auslastung = warnIfGt(utilization, 1.1, percent(utilization)),
          `SGBII(u.65)` = percent(sgbIIu65)
        )
      row.names(d) = 'values'
      t(d)
    }
  }, colnames=F, rownames=T, spacing='xs', sanitize.text.function = function(x) x, width='100%')

  output$selected_units = renderText({
    if (sum(r$units$selected) > 0) {
      selected_unit_ids = r$units$unit_id[r$units$selected]
      do.call(paste, c(sep=', ', as.list(selected_unit_ids)))
    } else {
      'Keine ausgewählt'
    }
  })
  
  output$selected_units_table = renderTable({
    if (sum(r$units$selected) > 0) {
      selected_units_data = r$units[r$units$selected,] %>% as.data.frame() %>%
        summarise(
          Anzahl=n(),
          Kinder=formatC(sum(population, na.rm=T), digits=2, format='f'),
          `SGBII(u.65)`=percent(sum(population*sgbIIu65, na.rm=T)/sum(population, na.rm=T))
        )
      row.names(selected_units_data) = 'values'
      t(selected_units_data)
    }
  }, colnames=F, rownames=T, spacing='xs', width='100%')
  
  ### optimization

  output$optimize_button = renderUI({
    if (r$running_optimization) {
      actionButton('optimize', label = 'Optimierung stoppen', icon = icon('stop'))
    } else {
      actionButton('optimize', label = 'Optimierung starten', icon = icon('play'))
    }
    
  })
  
  ### Export
  
  output$serveAssignment = downloadHandler(
    filename = function() {
      paste0('assignment_', Sys.Date(), '.csv')
    },
    content = function(con) {
      data = isolate(r$units) %>%
        as.data.frame() %>%
        select(unit_id, entity_id) %>%
        filter(entity_id != NO_ASSIGNMENT)
      write_csv(data, con)
    })
  
  output$serveGeoJSON = downloadHandler(
    filename = function() {
      paste0('assignment_', Sys.Date(), '.geojson')
    },
    content = function(con) {
      tmp_df = isolate(generate_spatial_df(r$units, r$entities, weights,
                                           NO_ASSIGNMENT))
      rgdal::writeOGR(tmp_df, con,
                      layer="entities", driver="GeoJSON",
                      verbose = TRUE, check_exists = FALSE)
    }
  )

  output$report = downloadHandler(
    filename = paste0('report_', Sys.Date(), '.pdf'),
    content = function(con) {
      temp_dir = tempdir()
      # load map
      map_path = 'data/berlin.rds'
      if (file.exists(map_path)) {
        berlin = read_rds(map_path)
      } else {
        berlin = ggmap::get_map('Berlin')
        write_rds(berlin, map_path, compress = 'gz')
      }
      isolate(rmarkdown::render(
        'templates/assignment_report_de.Rmd',
        output_file = con,
        intermediates_dir = temp_dir,
        envir = new.env(parent = globalenv()),  # isolate rendering
        params = list(
          map = berlin,
          units = r$units,
          entities = r$entities,
          NO_ASSIGNMENT = NO_ASSIGNMENT,
          min_util = MIN_UTILIZATION,
          max_util = MAX_UTILIZATION,
          colors = color_vec,
          optimizable_units = optimizable_units,
          weights = weights
        )
      ))
    }
  )
}

shinyApp(ui, server)
