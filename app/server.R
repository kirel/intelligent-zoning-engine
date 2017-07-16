source('deps.R')
source('ga.R', local=T)
source('io.R', local=T)
source('logging.R', local=T)
source('vars.R', local=T)

plan(multiprocess)

function(input, output, session) {
  
  ### Reactive Values
  
  r = reactiveValues(
    units=units,
    entities=entities,
    assignment_rev=0, # assignment revision - indicator that a block was reassigned manually
    map_rev=0, # map revision - indicator that the map needs an update
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
  
  ### TODO check for logged in user here!

  mapNeedsUpdate = function() {
    r$map_rev = r$map_rev + 1
  }

  tableNeedsUpdate = function() {
    r$assignment_rev = r$assignment_rev + 1
    mapNeedsUpdate()
  }

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
    index = assignment$list %>% detect_index(~ .$name == assignment$current)
    assignment$list[[index]]$assignment = r$units %>% as.data.frame() %>% select(unit_id, entity_id)
    flog.debug('Updating assignment %s', assignment$list[[index]]$name)
    tableNeedsUpdate()
    reset_optimization(r$units)
  })

  observeEvent(input$deassign_units, {
    flog.debug('Deassign button pressed')
    r$units[r$units$selected, 'entity_id'] = NONE_SELECTED
    r$units[r$units$selected, 'highlighted'] = T # FIXME wat?
    r$units[r$units$selected, 'updated'] = T
    index = assignment$list %>% detect_index(~ .$name == assignment$current)
    assignment$list[[index]]$assignment = r$units %>% as.data.frame() %>% select(unit_id, entity_id)
    flog.debug('Updating assignment %s', assignment$list[[index]]$name)
    tableNeedsUpdate()
    reset_optimization(r$units)
  })

  observeEvent(input$deselect_units, {
    flog.debug('Deselect button pressed')
    r$units[r$units$selected, 'updated'] = T
    r$units$selected = F
    tableNeedsUpdate()
  })

  observeEvent(input$lock_units, {
    flog.debug('Lock button pressed')
    r$units[r$units$selected, 'locked'] = T
    r$units[r$units$selected, 'updated'] = T
    tableNeedsUpdate()
    reset_optimization(r$units)
  })

  observeEvent(input$unlock_units, {
    flog.debug('Unlock button pressed')
    r$units[r$units$selected, 'locked'] = F
    r$units[r$units$selected, 'updated'] = T
    tableNeedsUpdate()
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

  observe({
    shinyjs::toggleClass("map-panel", "show-population", input$show_population)
  })

  observeEvent(input$show_population, {
    r$units$updated = T
    mapNeedsUpdate()
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
    return() # FIXME
    # TODO change to "save_data" function
    shinyStore::updateStore(session, "assignment", NULL)
    r$units = units
    tableNeedsUpdate()
  })

  observeEvent(r$assignment_rev, {
    return() # FIXME
    if (r$assignment_rev == 0) {
      try(isolate({
        # TODO change to "load_data" function
        assignment = input$store$assignment %>% charToRaw %>% unserialize
        stopifnot(c('unit_id', 'entity_id', 'locked') %in% colnames(assignment))
        assignemnt = r$units %>% as.data.frame() %>% select(unit_id) %>% left_join(assignment)
        r$units$entity_id = ifelse(is.na(assignment$entity_id), r$units$entity_id, assignment$entity_id)
        r$units$locked = ifelse(is.na(assignment$locked), r$units$locked, assignment$locked)
      }))
    }
    # TODO change to "save_data" function
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
    new_entities = r$units %>%
      as.data.frame() %>%
      select(unit_id) %>%
      left_join(upload, by="unit_id") %>% .$entity_id
    r$units$entity_id = ifelse(is.na(new_entities), NONE_SELECTED, new_entities)
    r$units$updated = TRUE
    tableNeedsUpdate()
  })
  
  ### assigments from collection
  observeEvent(assignment$current, {
    req(currentAssignment())
    req(currentAssignment()$assignment)
    flog.debug('Switched to assignment %s', assignment$current)
    flog.debug('currentAssignment()$name == %s', currentAssignment()$name)
    new_entities = r$units %>%
      as.data.frame() %>%
      select(unit_id) %>%
      left_join(currentAssignment()$assignment, by="unit_id") %>% .$entity_id
    r$units$entity_id = ifelse(is.na(new_entities), NONE_SELECTED, new_entities)
    r$units$updated = TRUE
    tableNeedsUpdate()
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
    mapNeedsUpdate()
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
      mapNeedsUpdate()
    })
  })

  ### Update the UI

  # UI is not actually updated.
  # Draw a polygon layer and another polygon "effects" or meta layer.
  # Each polygon has a class (that is actually an id) like unit_12345.
  # Then custom javascript just makes sure that all polygons have the right classes.
  updateMap = function(map, units, show_population = FALSE) {
    flog.debug('updateMap')
    flog.debug('nrow(units) %s', nrow(units))
    if (nrow(units) > 0) {
      flog.debug('redrawing units')
      replaceNA = function(x, replace) ifelse(is.na(x), replace, x)
      populationClasses = r$units %>% as.data.frame() %>%
        mutate(c=paste0(
          'population',
          10+10*as.integer(10*0.9*(population-min(population, na.rm=T))/(max(population, na.rm=T)-min(population, na.rm=T)))
        )) %>% .$c

      map = map %>%
        # redraw updated selected units
        addPolygons(
          data=units, group='units', layerId=~paste0('unit_', unit_id),
          smoothFactor = 0.2,
          stroke=NULL, color=NULL, fillOpacity=NULL,
          options=pathOptions(
            className=~paste(
              'unit',
              paste0('unit-', unit_id),
              populationClasses
            ))
        ) %>%
        # meta layer for effects
        addPolygons(
          data=units,
          group='meta_units', layerId=~paste0('meta_unit_', unit_id),
          smoothFactor = 0.2,
          stroke=NULL, color=NULL, fillOpacity=NULL,
          options=pathOptions(
            pointerEvents='none',
            className=~paste(
              'unit_meta',
              paste0('unit-', unit_id)
            ))
        )
    }
    # entities
    flog.debug('Calculating entities for map')
    flog.debug('Drawing entities for map')
    map = map %>%
      # capacity indicators
      addCircleMarkers(
        data=r$entities,
        group='entities-meta',
        layerId=~paste0('entity_meta_', entity_id),
        options=pathOptions(
          pointerEvents='none',
          className=~paste(
            'entity-meta',
            paste0('entity-', entity_id)
          )
        ),
        fillOpacity=NULL,
        color=NULL,
        opacity=NULL,
        fillColor=NULL,
        weight=2,
        radius=5
      ) %>%
      # entity markers
      addCircleMarkers(
        data=r$entities,
        group='entities',
        layerId=~paste0('entity_', entity_id),
        label = ~entity_id,
        options=pathOptions(
          className=~paste(
            'entity',
            paste0('entity-', entity_id)
          )
        ),
        fillOpacity = 1,
        color='black',
        opacity=1,
        weight=2,
        radius=5,
        fillColor=NULL
      )
    return(map)
  }

  updateMapJS = function() {
    utilization = entities_df %>%
      left_join(r$units %>% as.data.frame() %>% group_by(entity_id) %>% summarise(pop=sum(population)), by='entity_id') %>%
      mutate(utilization=pop/capacity)
    message = list(
      units=list(
        unit_id=as.list(r$units[r$units$updated,]$unit_id),
        entity_id=as.list(r$units[r$units$updated,]$entity_id),
        selected=as.list(r$units[r$units$updated,]$selected),
        locked=as.list(r$units[r$units$updated,]$locked)
      ),
      entities=list(
        entity_id=as.list(utilization$entity_id),
        utilization=as.list(utilization$utilization)
      ),
      selected_entity=r$selected_entity
    )
    session$onFlushed(function() { # run after leaflet
      session$sendCustomMessage(type = 'updateMap',
                                message = message)
    }, once=TRUE)
  }

  # Initial map render
  output$map <- renderLeaflet({
    isolate({
      m <- leaflet() %>%
        addProviderTiles("Stamen.Toner", option=providerTileOptions(opacity=0.2)) %>%  # Add default OpenStreetMap map tiles
        addPolylines(color='black', weight=4, opacity=1, data=bez) %>%
        updateMap(r$units, input$show_population)
      # initial caching of map markers
      session$onFlushed(function() { # run after leaflet
        session$sendCustomMessage(type = 'getMapLayers', message=list())
      }, once=TRUE)
      updateMapJS()
      r$units$updated = F
      flog.debug('Map initialized')
      m
    })
  })

  # incremental map update
  observeEvent(r$map_rev, {
    updateMapJS()
    r$units$updated = F
    return()
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
    if (resolved(ga$population_future) & (r$optimization_step != 0)) {
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
        if (resolved(ga$population_future) & (r$optimization_step == 0)) {
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

          tableNeedsUpdate()

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
    isolate(r$units) %>% as.data.frame() %>%
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
      DT::datatable(
        options=list(fixedHeader = T, processing = F, paging = F, searching = F, rowCallback = rowCallback, columnDefs=list(list(targets=c(2,3,5,6,7), class="dt-right"))),
        extensions='FixedHeader',
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
      HTML(paste0(r$selected_entity, '<span class="entity-color-indicator entity-bg-', r$selected_entity, '"></span> ', entity$SCHULNAME))
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

  output$serveAddresses = downloadHandler(
    filename = paste0('addresses_', Sys.Date(), '.csv'),
    content = function(con) {
      isolate({
        table = cbind(
          as.data.frame(addresses),
          over(addresses, r$units)
        ) %>% arrange(entity_id, street, no) %>%
          mutate(entity_id=ifelse(entity_id == NO_ASSIGNMENT, 'Keine', entity_id)) %>%
          select(Straße=street, Hausnummer=no, Schule=entity_id)

        write.csv( table, con)
      })
    }
  )
  output$addresses = downloadHandler(
    filename = function() { paste0('addresses_', Sys.Date(), '.pdf') },
    content = function(con) {
      temp_dir = tempdir()
      params = list(
        addresses = addresses,
        units = r$units,
        entities = r$entities,
        NO_ASSIGNMENT = NO_ASSIGNMENT
      )
      rmarkdown::render(
        'templates/addresses_report_de.Rmd',
        output_file = con,
        intermediates_dir = temp_dir,
        params = params,
        envir = new.env(parent = globalenv()) # isolate rendering
      )
    }
  )
  output$report = downloadHandler(
    filename = function() { paste0('report_', Sys.Date(), '.pdf') },
    content = function(con) {
      temp_dir = tempdir()
      # load map
      map_path = 'data/berlin.rds'
      if (file.exists(map_path)) {
        berlin = read_rds(map_path)
      } else {
        expand_bbox = function(bbox, buffer=0.1) {
          bbox+matrix(c(-0.1*(bbox[,'max']-bbox[,'min']),0.1*(bbox[,'max']-bbox[,'min'])), 2, 2)
        }
        berlin = get_map(expand_bbox(bbox(bez)), source='stamen', maptype = 'toner-lite')
        write_rds(berlin, map_path, compress = 'gz')
      }
      params = list(
        map = berlin,
        addresses = addresses,
        units = r$units,
        entities = r$entities,
        NO_ASSIGNMENT = NO_ASSIGNMENT,
        min_util = MIN_UTILIZATION,
        max_util = MAX_UTILIZATION,
        colors = color_vec,
        optimizable_units = optimizable_units,
        weights = weights
      )
      rmarkdown::render(
        'templates/assignment_report_de.Rmd',
        output_file = con,
        intermediates_dir = temp_dir,
        params = params,
        envir = new.env(parent = globalenv()) # isolate rendering
      )
    }
  )
  
  ## Scenarios
  
  scenario = reactiveValues(
    current = '2017',
    list = list(
      list(name='2017', entities=as_data_frame(entities@data), units=as_data_frame(units@data)),
      list(name='2018', entities=as_data_frame(entities@data), units=as_data_frame(units@data))
    )
  )
  
  currentScenario = reactive({
    scenario$list %>% detect(~ .$name == scenario$current)
  })
  
  observe({
    scenario$current = input$currentScenario
  })
  
  output$scenarioSelect = renderUI({
    selectizeInput('currentScenario', 'Szenario', map(scenario$list, ~ .$name), selected = scenario$current)
  })
  
  output$scenarios = renderUI({
    tags$ul(scenario$list %>% map(~ tags$li(paste(.$name, ifelse(.$name == scenario$current, '*', '')))))
  })
  
  output$serveScenarios = downloadHandler(
    filename = function() {
      paste0('scenarios_', Sys.Date(), '.xlsx')
    },
    content = function(con) {
      browser()
      scenario$list %>% reduce(function(acc, sc) {
        acc[[paste('entities', sc$name)]] = sc$entities
        acc[[paste('units', sc$name)]] = sc$units
        acc
      }, .init = list()) %>% write.xlsx(con)
    }
  )
  
  observeEvent(input$readScenarios, {
    flog.debug('reading scenarios')
    num_warn = length(warnings())

    data = getSheetNames(input$readScenarios$datapath) %>%
      set_names() %>%
      map(read_xlsx, path = input$readScenarios$datapath)
    scenario_list = names(data) %>% grep('units', ., value=T) %>%
      map(function(sheet_name) {
        scenario_name = substring(sheet_name, 7)
        list(
          name = scenario_name,
          units = data[[paste('units', scenario_name)]],
          entities = data[[paste('entities', scenario_name)]]
          )
      })
    scenario$list = scenario_list
    scenario$current = scenario_list[[1]]$name
        
    num_warn = length(warnings()) - num_warn
    validate(
      need(num_warn == 0,
           sprintf("Es gab %d Problem(e) mit der Datei.", num_warn))
      # need(),  # all unit_ids must be valid
      # need()  # all entity_ids must be valid
    )
  })
  
  ## Assignments
  
  assignment = reactiveValues(
    current = 'default',
    list = list(
      list(name='default', assignment=units@data %>% select(unit_id, entity_id)),
      list(name='alternative', assignment=units@data %>% select(unit_id, entity_id))
    )
  )
  
  currentAssignment = reactive({
    assignment$list %>% detect(~ .$name == assignment$current)
  })
  
  observe({
    assignment$current = input$currentAssignment
  })
  
  output$assignmentSelect = renderUI({
    selectizeInput('currentAssignment', 'Zuordnung', map(assignment$list, ~ .$name), selected = assignment$current)
  })
  
  output$assignments = renderUI({
    tags$ul(assignment$list %>% map(~ tags$li(paste(.$name, ifelse(.$name == assignment$current, '*', '')))))
  })
  
}
