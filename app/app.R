source('deps.R')
source('io.R')

plan(multiprocess)

con <- dbConnect(RSQLite::SQLite(), "data/communication.sqlite")

options(shiny.autoreload=T)
options(shiny.host="0.0.0.0")
options(warn=-1)

flog.threshold(DEBUG)
flog.appender(appender.file('optimization.log'), name='optimization')
flog.debug('Logging setup complete')

NONE_SELECTED = '__NONE_SELECTED__'
NO_ASSIGNMENT = NONE_SELECTED

# Load data
units = readOGR('data/units.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
entities = readOGR('data/entities.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
weights = read_csv('data/weights.csv')
adjacency = read_csv('data/adjacency.csv', col_types ='cc')

assignment = units@data %>%
  select(unit_id) %>%
  left_join(read_csv('data/assignment.csv'), by='unit_id') %>%
  mutate(entity_id = ifelse(is.na(entity_id), NO_ASSIGNMENT, entity_id))

units = units %>% sp::merge(assignment)

rm(assignment)

bez = readOGR('data/RBS_OD_BEZ_2015_12.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE) %>% subset(BEZ == '07')

entity_ids = unique(entities$entity_id)
unit_ids = unique(units$unit_id)
optimizable_units = units %>% as.data.frame %>% inner_join(weights, by='unit_id') %>% .$unit_id %>% unique

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
entity_colors = function(entity_id, desaturate) {
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
  tags$head(
    tags$link(rel="shortcut icon", href="http://idalab.de/favicon.ico"),
    tags$title(HTML("idalab - intelligent zoning engine")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
      tabsetPanel(type="tabs", id="tabs",
                  tabPanel("Details", div(id='detail',
                      fillRow(
                        div(id='detail--units',
                            h4(id='detail--units--selected-units', uiOutput('selected_units')),
                            div(id='detail--units--controls',
                                actionButton('deselect_units', '', icon=icon('remove')),
                                actionButton('assign_units', '', icon=icon('link')),
                                actionButton('deassign_units', '', icon=icon('unlink')),
                                actionButton('lock_units', '', icon=icon('lock')),
                                actionButton('unlock_units', '', icon=icon('unlock'))
                            ),
                            tableOutput('selected_units_table')
                        ),
                        div(id='detail--entity',
                            h4(textOutput('selected_entity')),
                            div(id='detail--units--controls',
                                actionButton('deselect_entity', '', icon=icon('remove'))
                            ),
                            tableOutput('selected_entity_table')
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
                                )
                  )),
                  tabPanel("Optimierung", div(id='optimize',
                      uiOutput('optimize_button', inline = TRUE),
                      plotOutput('fitness', height = '120px')
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
    running_optimization=FALSE,
    optimization_step=0,
    previous_mouseover=NONE_SELECTED)
  
  # variables for R optimization implementation
  ga = reactiveValues(
    last_timestamp = 0
    )

  ### Interaction

  observeEvent(input$optimize, {
    flog.debug('Optimize button pressed')
    if (r$running_optimization) {
      stop_optimization()
      r$running_optimization = FALSE
    } else {
      r$running_optimization = TRUE
      start_optimization(r$units %>% as.data.frame() %>% select(unit_id, entity_id, locked))
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
    reset_optimization(r$units)
  })
  
  observeEvent(input$unlock_units, {
    flog.debug('Unlock button pressed')
    r$units[r$units$selected, 'locked'] = F
    r$units[r$units$selected, 'updated'] = T
    reset_optimization(r$units)
  })
  
  observeEvent(input$deselect_entity, {
    flog.debug('Deselect button pressed')
    r$selected_entity = NONE_SELECTED
  }) 

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

  updateMap = function(map, units) {
    flog.debug('updateMap')
    flog.debug('nrow(units) %s', nrow(units))
    if (nrow(units) > 0) {
      flog.debug('redrawing units')
      map = map %>%
        # redraw updated selected units
        addPolygons(
          data=units, group='units', layerId=~paste0('unit_', unit_id),
          stroke = F, fillOpacity = 1, smoothFactor = 0.2,
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
    map = map %>%
      addCircleMarkers(
        data=r$entities, group='entities', layerId=~paste0('entity_', entity_id),
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
        updateMap(r$units)
      r$units$updated = F
      flog.debug('Map initialized')
      m      
    })
  })

  # incremental map update
  observe({
    updated_units = r$units[r$units$updated,]
    isolate({
      leafletProxy("map") %>% updateMap(updated_units)
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
    dbWriteTable(con, 'input', assignment, overwrite=T)
    dbSendQuery(con, 'INSERT INTO instructions VALUES ("start")')
  }
  
  reset_optimization = function(assignment) {
    if (r$running_optimization) {
      start_optimization(assignment)
    }
  }
  
  stop_optimization = function() {
    dbSendQuery(con, 'INSERT INTO instructions VALUES ("stop")')
  }
  
  is_optim_solution_updated = function() {
    solution_meta = dbReadTable(con, 'solution_meta', assignment)
    is.null(ga$last_timestamp) | solution_meta$timestamp > ga$last_timestamp
  }
  
  get_optim_solution = function() {
    solution_meta = dbReadTable(con, 'solution_meta', assignment)
    solution = dbReadTable(con, 'solution', assignment)
    ga$last_timestamp = solution_meta$timestamp
    list(solution=solution, score=solution_meta$score)
  }
  
  # main optimization loop
  observe({
    if (r$running_optimization) {
      flog.info('Ticking')
      isolate({
        
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

  ### table

  reactive_table_data = reactive({
    r$assignment_rev # recalculate if assignment_rev is altered
    data = isolate(r$units) %>% as.data.frame() %>%
      filter(entity_id != NO_ASSIGNMENT)

    # alternative data for staged assignment
    # FIXME add actual data
    alternative = data
    
    table_data = data %>%
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
      left_join(entities %>% as.data.frame %>% select(entity_id, capacity), by='entity_id') %>%
      mutate(
        utilization=pop/capacity
      )

    alternative_table_data = alternative %>%
      left_join(weights, by=c('unit_id', 'entity_id')) %>%
      group_by(entity_id) %>% summarise(
        num_units=n(),
        min_dist=min(min, na.rm=T),
        avg_dist=sum(population*avg, na.rm=T)/sum(population, na.rm=T), # population weighted mean
        max_dist=max(max, na.rm=T),
        pop=sum(population, na.rm=T),
        sgbIIu65=sum(population*sgbIIu65, na.rm=T)/sum(population, na.rm=T) # FIXME population is only kids...
      ) %>%
      left_join(entities %>% as.data.frame %>% select(entity_id, capacity), by='entity_id') %>%
      mutate(
        utilization=pop/capacity
      )

    diff = select(alternative_table_data, -entity_id) - select(table_data, -entity_id) # FIXME how is sorting ensured?

    table_data['delta_utilization'] = diff$utilization
    table_data %>%
      select(
        Schule=entity_id,
        Kapazität=capacity,
        Kinder=pop,
        Auslastung=utilization,
        #`ΔAusl.`=delta_utilization,
        `SGBII(u.65)`=sgbIIu65,
        #`Weg (min)`=min_dist,
        `Weg (Ø)`=avg_dist,
        `Weg (max)`=max_dist
      )

  })

  ### Outputs


  rowCallback = DT::JS("function(row, data) {",
"
  // if (data[4]+data[5] > 1) {
  if (data[4] > 1.1) {
    // $('td:eq(4), td:eq(5)', row).addClass('capacity-panic').removeClass('capacity-ok');
    $('td:eq(4)', row).addClass('capacity-panic').removeClass('capacity-ok');
  } else {
    // $('td:eq(4), td:eq(5)', row).addClass('capacity-ok').removeClass('capacity-panic');
    $('td:eq(4)', row).addClass('capacity-ok').removeClass('capacity-panic');
  }

/*
  if (data[5] > 0) {
    $('td:eq(5)', row).addClass('change-up').removeClass('change-down').removeClass('no-change');
  } else if (data[5] < 0) {
    $('td:eq(5)', row).addClass('change-down').removeClass('change-up').removeClass('no-change');
  } else {
    $('td:eq(5)', row).addClass('no-change').removeClass('change-down').removeClass('change-up');
  }
*/

  $('td:eq(4)', row).prepend('<i class=\"glyphicon glyphicon-ok\"></i><i class=\"fa glyphicon glyphicon-remove\"></i> ')
  //$('td:eq(5)', row).prepend('<i class=\"glyphicon glyphicon-arrow-up\"></i><i class=\"glyphicon glyphicon-arrow-down\"></i> ')
",
  "}")

  output$table = DT::renderDataTable({
    data = isolate(reactive_table_data())
    data %>% datatable(
        options=list(processing = F, paging = F, searching = F, rowCallback = rowCallback, columnDefs=list(list(targets=c(2,3,5,6,7), class="dt-right"))),
        selection=list(mode = 'single', selected = isolate(r$selected_school_index), target = 'row')
      ) %>%
      #formatPercentage(c('Auslastung', 'ΔAusl.', 'SGBII(u.65)'), digits = 2) %>%
      formatPercentage(c('Auslastung', 'SGBII(u.65)'), digits = 2) %>%
      formatRound(c(
        'Kinder',
        #'Weg (min)',
        'Weg (max)',
        'Weg (Ø)')) %>%
      formatStyle(
        'Weg (Ø)',
        background = styleColorBar(data$`Weg (Ø)`, 'lightskyblue'),
        backgroundSize = '92% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      # formatStyle(
      #   'Weg (min)',
      #   background = styleColorBar(data$`Weg (min)`, 'wheat'),
      #   backgroundSize = '92% 80%',
      #   backgroundRepeat = 'no-repeat',
      #   backgroundPosition = 'center'
      # ) %>%
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
    tableProxy %>% replaceData(reactive_table_data(), clearSelection='none')
  })

  observe({
    tableProxy %>% selectRows(r$selected_entity_index)
  })

  # Maybe via row callbacks? https://rstudio.github.io/DT/options.html
  
  ### Variables for detail views
  
  warnIfGt = function(num, thres, s) {
    ifelse(num > thres, paste0('<div class="warning">',s,'</div>'), s)
  }

  output$selected_entity = renderText({
    if (r$selected_entity != NONE_SELECTED) {
      entity = entities@data %>% filter(entity_id == r$selected_entity)
      paste(r$selected_entity, entity$SCHULNAME)
    } else {
      'Keine Schule ausgewählt'
    }
  })
  
  output$selected_entity_table = renderTable({
    if (r$selected_entity != NONE_SELECTED) {
      d = reactive_table_data()[reactive_table_data()$Schule == r$selected_entity,] %>%
        transmute(
          `Kapazität`=`Kapazität`,
          Kinder=warnIfGt(Kinder, `Kapazität`, formatC(Kinder, digits=2, format='f')),
          Auslastung=warnIfGt(Auslastung, 1.1, percent(Auslastung)),
          `SGBII(u.65)`=percent(`SGBII(u.65)`))
      row.names(d) = 'values'
      t(d)
    }
  }, colnames=F, rownames=T, spacing='xs', sanitize.text.function = function(x) x, width='100%')

  output$selected_units = renderText({
    if (sum(r$units$selected) > 0) {
      selected_unit_ids = r$units$unit_id[r$units$selected]
      do.call(paste, c(sep=', ', as.list(selected_unit_ids)))
    } else {
      'Keine Blöcke ausgewählt'
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
      # ensure write permissions, cf. http://shiny.rstudio.com/gallery/generating-reports.html
      temp_file = file.path(tempdir(), 'assignment_report_de.Rmd')
      file.copy('templates/assignment_report_de.Rmd', temp_file, overwrite = TRUE)
      # load map
      map_path = 'data/berlin.rds'
      if (file.exists(map_path)) {
        berlin = read_rds(map_path)
      } else {
        berlin = ggmap::get_map('Berlin')
        write_rds(berlin, map_path, compress = 'gz')
      }
      isolate(rmarkdown::render(
        temp_file,
        output_file = con,
        envir = new.env(parent = globalenv()),  # isolate rendering
        params = list(
          map = berlin,
          units = r$units,
          entities = r$entities,
          NO_ASSIGNMENT = NO_ASSIGNMENT,
          colors = color_vec,
          optimizable_units = optimizable_units,
          weights = weights
        )
      ))
    }
  )
}

shinyApp(ui, server)
