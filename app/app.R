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

# Load data
solution = read_rds('data/init_solution.rds') %>% select(BLK, school)
blocks = read_rds('data/blocks.rds') %>% sp::merge(solution)
bez = read_rds('data/bez.rds')
schools = read_rds('data/schools.rds')
block_stats = read_rds('data/block_stats.rds')
kids_in_blocks = block_stats %>% group_by(BLK) %>% summarise(num_kids=first(kids))
school_ids = unique(as.character(schools$spatial_name))
block_ids = unique(as.character(blocks$BLK))

# Prepare data
blocks[is.na(blocks$school),'school'] = ''
# takes blocks spdf and returns new school assignments
school_ids = as.character(schools$spatial_name)
block_ids = unique(as.character(block_stats$BLK)) # blocks with stats

# An individual is an assignment data frame BLK->school
# every gene is an assignment BLK->school

# preferrably sample blocks closer to the school
# should be higher for smaller numbers
distance_sample_weights = block_ids %>% map(function(block_id) {
  idx = block_stats$BLK == block_id
  avg = as.double(block_stats[idx,]$avg)
  school = block_stats[idx,]$dst
  # TODO make function and divide exponent by optimization step
  weights = (1-(avg-min(avg))/max(avg-min(avg)))^3
  set_names(weights, school)
}) %>% set_names(block_ids)

distance_sample_probs = function(block, heuristic_exponent=3) {
  weights = distance_sample_weights[[block]]
  pot_weights = weights^heuristic_exponent
  pot_weights/sum(pot_weights)
}

block_index = set_names(1:length(blocks$BLK), blocks$BLK)
blocks$selected = T # when the shool is selected
blocks$updated = F
schools$selected = T
schools$updated = F

# Scales
cfac = colorFactor(rainbow(length(school_ids)), levels=sample(school_ids))
school_colors = function(school_id, desaturate) {
  color = cfac(school_id)
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
      uiOutput('school'),
      #uiOutput('block'),
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
    blocks=blocks, schools=schools,
    assignment_rev=0, # assignment revision - indicator that a block was reassigned manually
    selected_block='', selected_school='', selected_school_index=NULL,
    running_optimization=FALSE,
    ga_population_future=future(NULL),
    optimization_step=0,
    previous_mouseover='')

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
    } else if (grepl('block_', input$map_shape_mouseover$id)) {
      # select the respective block
      r$selected_block = substring(input$map_shape_mouseover$id, 7)
    }
  })

  observeEvent(input$map_shape_mouseout, {
    req(input$map_shape_mouseout$id)
    if (grepl('block_', input$map_shape_mouseout$id)) {
      # deselect the respective block
      r$selected_block = ''
    }
  })

  # click on markers -> select school and blocks
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$id)
    clicked_marker = input$map_marker_click$id
    r$selected_school = ifelse(clicked_marker == isolate(r$selected_school), '', clicked_marker)
    index = schools$spatial_name %>% detect_index(~ .x == r$selected_school)
    if (is.null(index) | index == 0) r$selected_school_index = NULL else r$selected_school_index = index
    r$selected_block = ''
  })

  observe({
    row = input$table_rows_selected
    isolate({
      r$selected_school_index = row
      r$selected_school = ifelse(is.null(row), '', schools$spatial_name[row])
      r$selected_block = ''
    })
  })

  # click on shapes -> update the solution
  observeEvent(input$map_shape_click, {
    if (grepl('block_', input$map_shape_click$id)) {
      # a block was clicked
      clicked_block = sub('block_', '', input$map_shape_click$id)
      r$selected_block = clicked_block
      # update the solution
      currently_assigned_school = r$blocks[r$blocks$BLK == r$selected_block,]$school
      r$blocks[r$blocks$BLK == r$selected_block, 'school'] = ifelse(r$selected_school != currently_assigned_school, r$selected_school, '')
      r$blocks$updated = F
      r$blocks[r$blocks$BLK == r$selected_block, 'updated'] = T
      r$blocks[r$blocks$BLK == r$selected_block, 'selected'] = T
      r$assignment_rev = r$assignment_rev + 1

      add_current_to_ga_population()
    }
  })

  # school selection changed - update blocks
  observe({
    selected_school = r$selected_school
    isolate({
      schools_selected_before = r$schools$selected
      r$schools$selected = selected_school == ''
      r$schools[r$schools$spatial_name == selected_school, 'selected'] = T
      r$schools$updated = r$schools$updated | (r$schools$selected != schools_selected_before)
      blocks_selected_before = r$blocks$selected
      r$blocks$selected = selected_school == ''
      r$blocks[r$blocks$school == selected_school, 'selected'] = T
      r$blocks$updated = r$blocks$updated | (r$blocks$selected != blocks_selected_before)
    })
  })

  ### Update the UI

  # Initial map render
  output$map <- renderLeaflet({

    m <- leaflet() %>%
      addProviderTiles("Stamen.Toner", option=providerTileOptions(opacity=0.2)) %>%  # Add default OpenStreetMap map tiles
      addPolylines(color='black', weight=4, opacity=1, data=bez) %>%
      addPolygons(
        data=isolate(r$blocks), group='blocks', layerId=~paste0('block_',BLK),
        stroke = F, fillOpacity = 1, smoothFactor = 0.2, color= ~school_colors(school, !selected)
        ) %>%
      addCircleMarkers(
        data=isolate(r$schools), group='schools', layerId=~spatial_name,
        fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, fillColor= ~school_colors(spatial_name, !selected)
        )
    m

  })

  # incremental map update
  observe({
    updated_blocks = r$blocks[r$blocks$updated,]
    if (nrow(updated_blocks) > 0) {
      leafletProxy("map") %>%
        addPolygons(
          data=updated_blocks, group='blocks', layerId=~paste0('block_',BLK),
          stroke = F, fillOpacity = 1, smoothFactor = 0.2, color= ~school_colors(school, !selected)
        )
    }
    leafletProxy("map") %>%
      addCircleMarkers(
        data=r$schools, group='schools', layerId=~spatial_name,
        fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, fillColor= ~school_colors(spatial_name, !selected)
      )
    r$blocks$updated = F
    r$schools$updated = F
  })

  # block highlight marker
  observe({
    if (r$selected_block != '') {
      highlighted_block = r$blocks[block_index[r$selected_block],]
      leafletProxy("map") %>%
        addPolylines(data=highlighted_block, color='red', weight=4, layerId='highlighted_block') %>%
        addPolygons(
          data=highlighted_block, group='blocks', layerId=~paste0('block_',BLK),
          stroke = F, fillOpacity = 1, smoothFactor = 0.2, color= ~school_colors(school, !selected)
        ) %>%
        addCircleMarkers(
          data=r$schools, group='schools', layerId=~spatial_name,
          fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, fillColor= ~school_colors(spatial_name, !selected)
        )
    } else {
      leafletProxy("map") %>%
        removeShape('highlighted_block')
    }
  })

  ### optimization

  # Randomize (to test optimization)
  observeEvent(input$randomize, {
    random = blocks %>% as.data.frame() %>%
      filter(BLK %in% block_ids) %>%
      select(BLK, school) %>%
      mutate(school=sample(school_ids, nrow(.), replace = T))

    new_schools = r$blocks %>% as.data.frame() %>% select(BLK) %>% left_join(random, by="BLK") %>% .$school
    r$blocks$school = ifelse(is.na(new_schools), '', new_schools)
    r$blocks$updated = T

    r$ga_population = list(random)

    r$assignment_rev = r$assignment_rev + 1
  })

  # make sure there is at least one individual in the population
  # see main optimization loop
  add_current_to_ga_population = function() {
    # select only blocks with stats and assign random schools for unassigned blocks
    current = blocks %>% as.data.frame() %>%
      filter(BLK %in% block_ids) %>%
      select(BLK) %>% left_join(r$blocks %>% as.data.frame() %>% select(BLK, school), by='BLK') %>%
      mutate(school=ifelse(school == '', sample(school_ids, length(is.na(school)), replace = T), school))

    r$ga_population = list(current)
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
          future_population = value(r$ga_population_future)
          if (!is.null(future_population)) {
            r$ga_population = future_population
            
            fittest = r$ga_population[[1]]
            r$fittest_fitness = c(r$fittest_fitness, mem_fitness_f(fittest))
            
            # update relevant values for ui updates
            prev_schools = r$blocks$school
            new_schools = r$blocks %>% as.data.frame() %>% select(BLK) %>% left_join(fittest, by="BLK") %>% .$school
            r$blocks$school = ifelse(is.na(new_schools), '', new_schools)
            r$blocks$updated = r$blocks$school != prev_schools
            
            r$assignment_rev = r$assignment_rev + 1
            r$optimization_step = r$optimization_step + 1
          }
          
          heuristic_exponent = 20/(1+r$optimization_step)^(1) # TODO = 1/2?
          mutation_fraction = max(0.001, 1-r$optimization_step/3)
          cat('Running optimization step', r$optimization_step, 'with mutation_fraction', mutation_fraction, 'and heuristic_exponent', heuristic_exponent, '\n', file=stderr())
          
          r$ga_population_future = future({
            ga_select(
              ga_breed(r$ga_population,
                       fitness_f=mem_fitness_f,
                       mutation_fraction=mutation_fraction,
                       num_pairs = 200,
                       num_mutants = 200,
                       heuristic_exponent = heuristic_exponent),
              mem_fitness_f,
              max_population = 200)
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
    prob = individual %>% inner_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
      transmute(avg=avg, cost=(avg/max(avg))^heuristic_exponent, prob=cost/sum(cost)) %>% .$prob
    #cost = rep(1, nrow(individual))
    #cost = 1-cost
    mutation_idx = sample(1:nrow(individual), num_mutations, prob = prob)
    mutation_blks = as.character(individual[mutation_idx, 'BLK'])
    # preferrably select schools closer to the block
    mutations = mutation_blks %>% map(~ sample(names(distance_sample_probs(.x, heuristic_exponent)), 1, replace=T, prob=distance_sample_probs(.x, heuristic_exponent)))
    individual[mutation_idx, 'school'] = unlist(mutations)
    individual
  }

  # randomly mix assignments from two individuals into two new ones
  # TODO only where they are different
  ga_crossover = function(a, b) {
    # only select from genes that are different
    difference = a$school != b$school
    if (sum(difference)==0) return(list(a, b))
    child_a = a[,]
    child_b = b[,]
    fraction = runif(1)
    num_genes = ceiling(fraction*sum(difference))
    swap_idx = sample((1:nrow(a))[difference], num_genes)
    child_a[swap_idx, 'school'] = b[swap_idx, 'school']
    child_b[swap_idx, 'school'] = a[swap_idx, 'school']
    list(child_a, child_b)
  }

  # takes a list of individuals and breeds new ones in addition
  # returns the new population (that includes the old one)
  ga_breed = function(population, fitness_f, num_pairs = 50, num_mutants = 100, mutation_fraction=0.001, heuristic_exponent=3) {
    # precalculate sampling weights for sampling from the fittest
    fitness = unlist(map(population, fitness_f))
    cat('original population fitness', summary(fitness), '\n', file=stderr())
    weights = -fitness+min(fitness)+max(fitness)
    prob = weights/sum(weights)

    cat('Mutating', num_mutants, 'individuals\n', file=stderr())
    mutated = population %>%
      sample(num_mutants, replace = T, prob = prob) %>% # sample based on fitness
      map(~ ga_mutate(.x, mutation_fraction, heuristic_exponent))
    if (length(mutated)>0) cat('mutated fitness', summary(unlist(map(mutated, fitness_f))), '\n', file=stderr())
    
    mating_population = c(population, mutated)
    if (length(mating_population) > 1) {
      mating_fitness = unlist(map(mating_population, fitness_f))
      mating_weights = -mating_fitness+min(mating_fitness)+max(mating_fitness)
      mating_prob = mating_weights/sum(mating_weights)
      
      cat('Mating', num_pairs, 'pairs\n', file=stderr())
      pairs = (1:num_pairs) %>% map(~ sample(mating_population, 2, prob = mating_prob)) # sample based on fitness
      mated = do.call(c, map(pairs, ~ do.call(ga_crossover, .x)))
      cat('mated fitness', summary(unlist(map(mated, fitness_f))), '\n', file=stderr())
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
    cat('Population size', length(population), '\n', file=stderr())
    population = sort_by(unique(population), fitness_f)
    cat(length(population), 'unique\n', file=stderr())
    num_survivors = min(ceiling(length(population)*survival_fraction), max_population)
    cat('Keeping', num_survivors, 'survivors\n', file=stderr())
    #survivors = population[rank(unlist(fitness), t = 'r') <= num_survivors]
    survivors = population[1:num_survivors]
    survivors
  }

  ### /genetic algorithm

  fitness_f = function(individual) {
    OVER_CAPACITY_PENALTY = 1
    UNDER_CAPACITY_PENALTY = 0.1
    DIST_WEIGHT = 0.0005
    OVER_CAPACITY_WEIGHT = 1
    UNDER_CAPACITY_WEIGHT = 1
    individual %>% inner_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
      group_by(school) %>%
      summarise(kids=sum(kids), Kapa=first(Kapa), avg=mean(avg^2)) %>%
      #summarise(kids=sum(kids), Kapa=first(Kapa), avg=sum(avg^2*kids)) %>%
      rowwise() %>% mutate(over_capacity=max(1, kids-Kapa), under_capacity=max(1, Kapa-kids)) %>% ungroup %>%
      mutate(over_capacity_penalty=(over_capacity*OVER_CAPACITY_PENALTY)^2, under_capacity_penalty=(under_capacity*UNDER_CAPACITY_PENALTY)^2) %>%
      summarise(avg=sum(avg), over_capacity_penalty=sum(over_capacity_penalty), under_capacity_penalty=sum(under_capacity_penalty)) %>%
      (function(x) {
        if (runif(1)<0) {
        #if (runif(1)<0.1) {
          cat('Distance error:', DIST_WEIGHT*x$avg, '\n', file=stderr())
          cat('Over capacity error:', OVER_CAPACITY_WEIGHT*x$over_capacity_penalty, '\n', file=stderr())
          cat('Under capacity error:', UNDER_CAPACITY_WEIGHT*x$under_capacity_penalty, '\n', file=stderr())
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
    r$assignment_rev # recalculate of assignment_rev is altered
    data = isolate(r$blocks) %>% as.data.frame() %>%
      mutate(school=ifelse(school == '', 'Keine', school))

    alternative = data
    if (r$selected_block != '') {
      currently_assigned_school = alternative[block_index[r$selected_block],]$school
      alternative[block_index[r$selected_block], 'school'] = ifelse(r$selected_school != currently_assigned_school, r$selected_school, '')
      alternative = alternative %>% mutate(school=ifelse(school == '', 'Keine', school))
    }

    table_data = data %>%
      left_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
      left_join(kids_in_blocks, by='BLK') %>%
      group_by(school) %>% summarise(
        kids=sum(num_kids, na.rm=T),
        num_blocks=n(),
        min_time=min(min, na.rm=T),
        avg_time=mean((kids*avg)/sum(kids, na.rm=T), na.rm=T),
        max_time=max(max, na.rm=T),
        Kapa=first(Kapa)
      ) %>%
      mutate(
        utilization=kids/Kapa
      )

    alternative_table_data = alternative %>%
      left_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
      left_join(kids_in_blocks, by='BLK') %>%
      group_by(school) %>% summarise(
        kids=sum(num_kids, na.rm=T),
        num_blocks=n(),
        min_time=min(min, na.rm=T),
        avg_time=mean((kids*avg)/sum(kids, na.rm=T), na.rm=T),
        max_time=max(max, na.rm=T),
        Kapa=first(Kapa)
      ) %>%
      mutate(
        utilization=kids/Kapa
      )

    diff = select(alternative_table_data, -school) - select(table_data, -school)

    table_data['delta_utilization'] = diff$utilization
    table_data %>%
      select(
        Schule=school,
        Kapazität=Kapa,
        Kinder=kids,
        Auslastung=utilization,
        `ΔAusl.`=delta_utilization,
        `Weg (min)`=min_time,
        `Weg (Ø)`=avg_time,
        `Weg (max)`=max_time
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
    tableProxy %>% selectRows(r$selected_school_index)
  })

  # Maybe via row callbacks? https://rstudio.github.io/DT/options.html

  output$school = renderUI({
    if (r$selected_school == '') {
      div(h4('Keine Schule ausgewählt'))
    } else {
      school = schools %>% filter(spatial_name == r$selected_school)
      div(h4(paste(r$selected_school, school$SCHULNAME)))
    }
  })

  output$block = renderUI({
    div(h4(r$selected_block))
  })

  output$optimize = renderUI({
    actionButton('optimize', label = ifelse(r$running_optimization, 'Optimierung stoppen', 'Optimierung starten'))
  })

}

shinyApp(ui, server)
