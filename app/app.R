library(rgdal)
library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmltools)
library(purrr)
library(colorspace)
library(ggplot2)
library(shiny)
library(DT)
library(memoise)
library(future)

plan(multiprocess)

options(shiny.autoreload=T)
options(warn=-1)

LOG='debug.log' # stderr()

NONE_SELECTED = '__NONE_SELECTED__'
NO_ASSIGNMENT = NONE_SELECTED

# Load data
units = readOGR('data/units.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
entities = readOGR('data/entities.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
weights = read_csv('data/weights.csv')

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

### Helper functions

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

# when a unit/entity is selected it is not desaturated
units$selected = T
units$updated = F
entities$selected = T
entities$updated = F

# Scales
cfac = colorFactor(rainbow(length(entity_ids)), levels=sample(entity_ids))
entity_colors = function(entity_id, desaturate) {
  color = cfac(entity_id)
  ifelse(desaturate, desat(color, 0.2), color)
}

desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}

### UI
ui <- fillPage(
  tags$head(
    tags$link(rel="shortcut icon", href="http://idalab.de/favicon.ico"),
    tags$title(HTML("idalab - intelligent zoning engine")),
    tags$style(HTML(
    "
    #map-panel { height: calc(100% - 240px); }
    #table-panel { height: 100%; overflow: scroll; }
    .capacity-ok { color: green; }
    .capacity-ok .glyphicon-remove { display: none; }
    .capacity-panic { color: red; }
    .capacity-panic .glyphicon-ok { display: none; }
    .change-up .glyphicon-arrow-down { display: none; }
    .change-down .glyphicon-arrow-up { display: none; }
    .no-change { color: transparent; }
    "))),
  fillRow(
    div(
      id='map-panel',
      leafletOutput("map", width="100%", height='100%'),
      uiOutput('entity'),
      #uiOutput('unit'),
      uiOutput('optimize', inline = TRUE),
      actionButton('randomize', 'Randomisieren'),
      plotOutput('fitness', height = '150px')
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

  r <- reactiveValues(
    units=units, entities=entities,
    assignment_rev=0, # assignment revision - indicator that a block was reassigned manually
    selected_unit=NONE_SELECTED, selected_entity=NONE_SELECTED, selected_entity_index=NULL,
    running_optimization=FALSE,
    ga_population_future=future(NULL),
    optimization_step=0,
    previous_mouseover=NONE_SELECTED)

  ### Interaction

  observeEvent(input$optimize, {
    if (r$running_optimization) {
      r$running_optimization = FALSE
    } else {
      r$running_optimization = TRUE
    }
  })

  # block mouseover -> highlight the shape
  observe({
    req(input$map_shape_mouseover$id)
    if (input$map_shape_mouseover$id != isolate(r$previous_mouseover)) {
      # run this again and only set selected block if we're over the same shape after 100ms
      r$previous_mouseover = input$map_shape_mouseover$id
      invalidateLater(100, session)
    } else if (grepl('unit_', input$map_shape_mouseover$id)) {
      # select the respective block
      r$selected_unit = substring(input$map_shape_mouseover$id, 6) # FIXME stattdessen sub('unit_', '', ...)?
    }
  })

  observeEvent(input$map_shape_mouseout, {
    req(input$map_shape_mouseout$id)
    if (grepl('unit_', input$map_shape_mouseout$id)) {
      # deselect the respective block
      r$selected_block = NONE_SELECTED
    }
  })

  # click on markers -> select school and blocks
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$id)
    clicked_marker = input$map_marker_click$id
    r$selected_entity = ifelse(clicked_marker == isolate(r$selected_entity), NONE_SELECTED, clicked_marker)
    index = entities$entity_id %>% detect_index(~ .x == r$selected_entity)
    if (is.null(index) | index == 0) r$selected_entity_index = NULL else r$selected_entity_index = index
    r$selected_unit = NONE_SELECTED
  })

  # click on table row
  observe({
    row = input$table_rows_selected
    isolate({
      r$selected_entity_index = row
      r$selected_entity = ifelse(is.null(row), NONE_SELECTED, entities$entity_id[row])
      r$selected_unit = NONE_SELECTED
    })
  })

  # click on shapes -> update the solution
  observeEvent(input$map_shape_click, {
    if (grepl('unit_', input$map_shape_click$id)) {
      # a block was clicked
      clicked_unit = sub('unit_', '', input$map_shape_click$id)
      r$selected_unit = clicked_unit
      # update the solution
      currently_assigned_entity = r$units[r$units$unit_id == r$selected_unit,]$entity_id
      # FIXME 3 times indexing the same thing - can this be faster?
      r$units[r$units$unit_id == r$selected_unit, 'entity_id'] = ifelse(r$selected_entity != currently_assigned_entity, r$selected_entity, NONE_SELECTED)
      r$units$updated = F
      r$units[r$units$unit_id == r$selected_unit, 'updated'] = T
      r$units[r$units$unit_id == r$selected_unit, 'selected'] = T
      r$assignment_rev = r$assignment_rev + 1

      # cancel optimization step (which is now invalid)
      resetOptimization()
    }
  })

  # school selection changed - update blocks
  observe({
    selected_entity = r$selected_entity
    isolate({
      entities_selected_before = r$entities$selected
      r$entities$selected = selected_entity == NONE_SELECTED # if nothing is selected all are selected
      r$entities[r$entities$entity_id == selected_entity, 'selected'] = T
      r$entities$updated = r$entities$updated | (r$entities$selected != entities_selected_before)
      units_selected_before = r$units$selected
      r$units$selected = selected_entity == NONE_SELECTED # if nothing is selected all are selected
      r$units[r$units$entity_id == selected_entity, 'selected'] = T
      r$units$updated = r$units$updated | (r$units$selected != units_selected_before)
    })
  })

  ### Update the UI

  # Initial map render
  output$map <- renderLeaflet({

    m <- leaflet() %>%
      addProviderTiles("Stamen.Toner", option=providerTileOptions(opacity=0.2)) %>%  # Add default OpenStreetMap map tiles
      addPolylines(color='black', weight=4, opacity=1, data=bez) %>%
      addPolygons(
        data=isolate(r$units), group='units', layerId=~paste0('unit_', unit_id),
        stroke = F, fillOpacity = 1, smoothFactor = 0.2, color= ~entity_colors(entity_id, !selected)
        ) %>%
      addCircleMarkers(
        data=isolate(r$entities), group='entities', layerId=~entity_id,
        fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, fillColor= ~entity_colors(entity_id, !selected)
        )
    m

  })

  # incremental map update
  observe({
    updated_units = r$units[r$units$updated,]
    if (nrow(updated_units) > 0) {
      leafletProxy("map") %>%
        addPolygons(
          data=updated_units, group='units', layerId=~paste0('unit_', unit_id),
          stroke = F, fillOpacity = 1, smoothFactor = 0.2, color= ~entity_colors(entity_id, !selected)
        )
    }
    leafletProxy("map") %>%
      addCircleMarkers(
        data=r$entities, group='entities', layerId=~entity_id,
        fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, fillColor= ~entity_colors(entity_id, !selected)
      )
    r$units$updated = F
    r$entities$updated = F
  })

  # block highlight marker
  observe({
    if (r$selected_unit != NONE_SELECTED) {
      highlighted_unit = r$units[unit_index[r$selected_unit],]
      leafletProxy("map") %>%
        addPolylines(data=highlighted_unit, color='red', weight=4, layerId='highlighted_unit') %>%
        addPolygons(
          data=highlighted_unit, group='units', layerId=~paste0('unit_', unit_id),
          stroke = F, fillOpacity = 1, smoothFactor = 0.2, color= ~entity_colors(entity_id, !selected)
        ) %>%
        addCircleMarkers(
          data=r$entities, group='entities', layerId=~entity_id,
          fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, fillColor= ~entity_colors(entity_id, !selected)
        )
    } else {
      leafletProxy("map") %>%
        removeShape('highlighted_unit')
    }
  })

  ### optimization

  # Randomize (to test optimization)
  observeEvent(input$randomize, {
    random = units %>% as.data.frame() %>%
      filter(unit_id %in% optimizable_units) %>%
      select(unit_id, entity_id) %>%
      mutate(entity_id=sample(entity_ids, nrow(.), replace = T))

    new_entities = r$units %>% as.data.frame() %>% select(unit_id) %>% left_join(random, by="unit_id") %>% .$entity_id
    r$units$entity_id = ifelse(is.na(new_entities), NO_ASSIGNMENT, new_entities)
    r$units$updated = T

    r$ga_population = list(random)

    r$assignment_rev = r$assignment_rev + 1
  })

  # make sure there is at least one individual in the population
  # see main optimization loop
  add_current_to_ga_population = function() {
    # select only blocks with stats and assign random schools for unassigned blocks
    current = units %>% as.data.frame() %>%
      filter(unit_id %in% optimizable_units) %>%
      select(unit_id) %>% left_join(r$units %>% as.data.frame() %>% select(unit_id, entity_id), by='unit_id') %>%
      mutate(entity_id=ifelse(entity_id == NO_ASSIGNMENT, sample(entity_ids, length(is.na(entity_id)), replace = T), entity_id))

    r$ga_population = list(current)
  }
  
  resetOptimization = function() {
    cat('Resetting optimization before step', r$optimization_step, 'finished\n', file=stderr())
    r$optimization_step = 0
    # r$ga_population_future = future(NULL) # FIXME this causes a lot of futures to be calculated in parallel
    forget(mem_fitness_f)
    add_current_to_ga_population()
  }

  # main optimization loop
  observe({
    if (r$running_optimization) {
      isolate({
        # if there is no population to iterate on
        if (is.null(r$ga_population)) {
          add_current_to_ga_population()
        }
        
        # if there is a resolved ga_population_future take the result and update all the things, then start a new one
        if (resolved(r$ga_population_future)) {
          cat('Finished optimization step', r$optimization_step, '\n', file=stderr())
          future_population = value(r$ga_population_future)
          if (r$optimization_step > 0 && !is.null(future_population)) {
            r$ga_population = future_population
            
            fittest = r$ga_population[[1]]
            r$fittest_fitness = c(r$fittest_fitness, mem_fitness_f(fittest))
            
            # update relevant values for ui updates
            prev_entities = r$units$entity_id
            new_entities = r$units %>% as.data.frame() %>% select(unit_id) %>% left_join(fittest, by="unit_id") %>% .$entity_id
            r$units$entity_id = ifelse(is.na(new_entities), prev_entities, new_entities)
            r$units$updated = r$units$entity_id != prev_entities
            
            r$assignment_rev = r$assignment_rev + 1
          }
          
          r$optimization_step = r$optimization_step + 1
          heuristic_exponent = 20/r$optimization_step^(1) # TODO = 1/2?
          mutation_fraction = max(0.001, 1-(r$optimization_step-1)/3)
          cat('Running optimization step', r$optimization_step, 'with mutation_fraction', mutation_fraction, 'and heuristic_exponent', heuristic_exponent, '\n', file=stderr())
          
          r$ga_population_future = future({
            ga_select(
              ga_breed(r$ga_population,
                       fitness_f=mem_fitness_f,
                       mutation_fraction=mutation_fraction,
                       num_pairs = 50,
                       num_mutants = 50,
                       heuristic_exponent = heuristic_exponent),
              mem_fitness_f,
              max_population = 50)
          })  
        } # otherwise just skip and do nothing in this iteration
      })
      invalidateLater(100, session)
    } else {
      r$optimization_step = 0
      forget(mem_fitness_f)
    }
  })

  output$fitness = renderPlot({
    ggplot() +
      geom_line(aes(x=seq_along(r$fittest_fitness), y=r$fittest_fitness)) +
      geom_line(aes(x=seq_along(diff(r$fittest_fitness))+1, y=diff(r$fittest_fitness)*(-1)), linetype=2) +
      expand_limits(y=0) + labs(x = 'Optimierungsschritt', y = 'Kostenfunktion')
  })

  ### genetic algorithm
  
  # randomly reassign a number of schools
  ga_mutate = function(individual, fraction=0.05, heuristic_exponent=3) {
    if (fraction == 0) return(individual)
    num_mutations = ceiling(fraction*nrow(individual))
    # preferrably select high costs blocks to mutate # FIXME does this hurt dense areas?
    # TODO divide exponent by optimization step
    prob = individual %>% inner_join(weights, by=c('unit_id', 'entity_id')) %>%
      transmute(avg=avg, cost=(avg/max(avg))^heuristic_exponent, prob=cost/sum(cost)) %>% .$prob
    #cost = rep(1, nrow(individual))
    #cost = 1-cost
    mutation_idx = sample(1:nrow(individual), num_mutations, prob = prob)
    mutation_units = as.character(individual[mutation_idx, 'unit_id'])
    # preferrably select schools closer to the block
    mutations = mutation_units %>% map(~ sample(names(distance_sample_probs(.x, heuristic_exponent)), 1, replace=T, prob=distance_sample_probs(.x, heuristic_exponent)))
    individual[mutation_idx, 'entity_id'] = unlist(mutations)
    individual
  }

  # randomly mix assignments from two individuals into two new ones
  # TODO only where they are different
  ga_crossover = function(a, b) {
    # only select from genes that are different
    difference = a$entity_id != b$entity_id
    if (sum(difference)==0) return(list(a, b))
    child_a = a[,]
    child_b = b[,]
    fraction = runif(1)
    num_genes = ceiling(fraction*sum(difference))
    swap_idx = sample((1:nrow(a))[difference], num_genes)
    child_a[swap_idx, 'entity_id'] = b[swap_idx, 'entity_id']
    child_b[swap_idx, 'entity_id'] = a[swap_idx, 'entity_id']
    list(child_a, child_b)
  }

  # takes a list of individuals and breeds new ones in addition
  # returns the new population (that includes the old one)
  ga_breed = function(population, fitness_f, num_pairs = 50, num_mutants = 100, mutation_fraction=0.001, heuristic_exponent=3) {
    # precalculate sampling weights for sampling from the fittest
    fitness = unlist(map(population, fitness_f))
    cat('original population fitness', summary(fitness), '\n', file=LOG)
    weights = -fitness+min(fitness)+max(fitness)
    prob = weights/sum(weights)

    cat('Mutating', num_mutants, 'individuals\n', file=stderr())
    mutated = population %>%
      sample(num_mutants, replace = T, prob = prob) %>% # sample based on fitness
      map(~ ga_mutate(.x, mutation_fraction, heuristic_exponent))
    if (length(mutated)>0) cat('mutated fitness', summary(unlist(map(mutated, fitness_f))), '\n', file=LOG)
    
    mating_population = c(population, mutated)
    if (length(mating_population) > 1) {
      mating_fitness = unlist(map(mating_population, fitness_f))
      mating_weights = -mating_fitness+min(mating_fitness)+max(mating_fitness)
      mating_prob = mating_weights/sum(mating_weights)
      
      cat('Mating', num_pairs, 'pairs\n', file=LOG)
      pairs = (1:num_pairs) %>% map(~ sample(mating_population, 2, prob = mating_prob)) # sample based on fitness
      mated = do.call(c, map(pairs, ~ do.call(ga_crossover, .x)))
      cat('mated fitness', summary(unlist(map(mated, fitness_f))), '\n', file=LOG)
    } else {
      mated = list()
    }
    
    # original population, mutated population and mated population
    new_population = c(
      population,
      mutated,
      mated
    )
  }

  # selects the fittest individuals according to a fitness function
  ga_select = function(population, fitness_f, survival_fraction=1, max_population=50) {
    cat('Population size', length(population), '\n', file=LOG)
    population = sort_by(unique(population), fitness_f)
    cat(length(population), 'unique\n', file=LOG)
    num_survivors = min(ceiling(length(population)*survival_fraction), max_population)
    cat('Keeping', num_survivors, 'survivors\n', file=LOG)
    #survivors = population[rank(unlist(fitness), t = 'r') <= num_survivors]
    survivors = population[1:num_survivors]
    survivors
  }

  ### /genetic algorithm

  fitness_f = function(individual) {
    OVER_CAPACITY_PENALTY = 1
    UNDER_CAPACITY_PENALTY = 1
    DIST_WEIGHT = 1/2000^2 # 2000m means penalty of 1
    OVER_CAPACITY_WEIGHT = 1/20/10 # 20 over capacity means penalty of 0.1
    UNDER_CAPACITY_WEIGHT = 1/20/10 # 20 under capacity means penalty of 0.1
    individual %>% inner_join(units %>% as.data.frame %>% select(unit_id, population), by='unit_id') %>% # FIXME faster?
      inner_join(weights, by=c('unit_id', 'entity_id')) %>%
      group_by(entity_id) %>%
      summarise(avg=mean(avg^2), population=sum(population)) %>%
      inner_join(entities %>% as.data.frame %>% select(entity_id, capacity), by='entity_id') %>% # FIXME faster?
      rowwise() %>% mutate(over_capacity=max(1, population-capacity), under_capacity=max(1, capacity-population)) %>% ungroup %>%
      mutate(over_capacity_penalty=(over_capacity*OVER_CAPACITY_PENALTY)^2, under_capacity_penalty=(under_capacity*UNDER_CAPACITY_PENALTY)^2) %>%
      summarise(avg=mean(avg), over_capacity_penalty=mean(over_capacity_penalty), under_capacity_penalty=mean(under_capacity_penalty)) %>%
      (function(x) {
        #if (runif(1)<0) {
        if (runif(1)<0.01) {
          cat('Distance error:', DIST_WEIGHT*x$avg, '\n', file=LOG)
          cat('Over capacity error:', OVER_CAPACITY_WEIGHT*x$over_capacity_penalty, '\n', file=LOG)
          cat('Under capacity error:', UNDER_CAPACITY_WEIGHT*x$under_capacity_penalty, '\n', file=LOG)
        }
        x
      }) %>%
      mutate(fitness = DIST_WEIGHT*avg +
               OVER_CAPACITY_WEIGHT*over_capacity_penalty +
               UNDER_CAPACITY_WEIGHT*under_capacity_penalty) %>% .$fitness
  }

  mem_fitness_f = memoise(fitness_f)

  ### table

  reactive_table_data = reactive({
    r$assignment_rev # recalculate if assignment_rev is altered
    data = isolate(r$units) %>% as.data.frame() %>%
      filter(entity_id != NO_ASSIGNMENT)

    # alternative data for hovered unit
    alternative = data
    if (r$selected_unit != NONE_SELECTED) {
      currently_assigned_entity = alternative[unit_index[r$selected_unit],]$entity_id
      alternative[unit_index[r$selected_unit], 'entity_id'] = ifelse(r$selected_entity != currently_assigned_entity, r$selected_entity, NO_ASSIGNMENT)
      alternative = alternative %>% filter(entity_id != NO_ASSIGNMENT)
    }
    
    table_data = data %>%
      left_join(weights, by=c('unit_id', 'entity_id')) %>%
      group_by(entity_id) %>% summarise(
        num_units=n(),
        min_dist=min(min, na.rm=T),
        avg_dist=mean((population*avg)/sum(population, na.rm=T), na.rm=T),
        max_dist=max(max, na.rm=T),
        pop=sum(population, na.rm=T)
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
        avg_dist=mean((population*avg)/sum(population, na.rm=T), na.rm=T),
        max_dist=max(max, na.rm=T),
        pop=sum(population, na.rm=T)
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
        `ΔAusl.`=delta_utilization,
        `Weg (min)`=min_dist,
        `Weg (Ø)`=avg_dist,
        `Weg (max)`=max_dist
      )

  })

  ### Outputs


  rowCallback = DT::JS("function(row, data) {",
"
  if (data[4]+data[5] > 1) {
    $('td:eq(4), td:eq(5)', row).addClass('capacity-panic').removeClass('capacity-ok');
  } else {
    $('td:eq(4), td:eq(5)', row).addClass('capacity-ok').removeClass('capacity-panic');
  }

  if (data[5] > 0) {
    $('td:eq(5)', row).addClass('change-up').removeClass('change-down').removeClass('no-change');
  } else if (data[5] < 0) {
    $('td:eq(5)', row).addClass('change-down').removeClass('change-up').removeClass('no-change');
  } else {
    $('td:eq(5)', row).addClass('no-change').removeClass('change-down').removeClass('change-up');
  }

  $('td:eq(4)', row).prepend('<i class=\"glyphicon glyphicon-ok\"></i><i class=\"fa glyphicon glyphicon-remove\"></i> ')
  $('td:eq(5)', row).prepend('<i class=\"glyphicon glyphicon-arrow-up\"></i><i class=\"glyphicon glyphicon-arrow-down\"></i> ')
",
  "}")

  output$table = DT::renderDataTable({
    data = isolate(reactive_table_data())
    data %>% datatable(
        options=list(processing = F, paging = F, searching = F, rowCallback = rowCallback, columnDefs=list(list(targets=c(2,3,5,6,7), class="dt-right"))),
        selection=list(mode = 'single', selected = isolate(r$selected_school_index), target = 'row')
      ) %>%
      formatPercentage(c('Auslastung', 'ΔAusl.'), digits = 2) %>%
      formatRound(c('Kinder', 'Weg (min)', 'Weg (max)', 'Weg (Ø)')) %>%
      formatStyle(
        'Weg (Ø)',
        background = styleColorBar(data$`Weg (Ø)`, 'lightskyblue'),
        backgroundSize = '92% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Weg (min)',
        background = styleColorBar(data$`Weg (min)`, 'wheat'),
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
    tableProxy %>% replaceData(reactive_table_data(), clearSelection='none')
  })

  observe({
    tableProxy %>% selectRows(r$selected_entity_index)
  })

  # Maybe via row callbacks? https://rstudio.github.io/DT/options.html

  output$entity = renderUI({
    if (r$selected_entity == NONE_SELECTED) {
      div(h4('Keine Schule ausgewählt'))
    } else {
      entity = entities@data %>% filter(entity_id == r$selected_entity)
      div(h4(paste(r$selected_entity, entity$SCHULNAME)))
    }
  })

  output$unit = renderUI({
    div(h4(r$selected_unit))
  })

  output$optimize = renderUI({
    actionButton('optimize', label = ifelse(r$running_optimization, 'Optimierung stoppen', 'Optimierung starten'))
  })

}

shinyApp(ui, server)
