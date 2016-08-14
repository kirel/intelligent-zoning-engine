library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmltools)
library(purrr)
library(colorspace)
library(DT)
library(shiny)

# Load data
solution = read_rds('init_solution.rds') %>% select(BLK, school)
blocks = read_rds('blocks.rds') %>% sp::merge(solution)
bez = read_rds('bez.rds')
schools = read_rds('schools.rds')
block_stats = read_rds('block_stats.rds')
kids_in_blocks = block_stats %>% group_by(BLK) %>% summarise(num_kids=first(kids))
school_ids = unique(as.character(schools$spatial_name))
block_ids = unique(as.character(blocks$BLK))

# Prepare data
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
  tags$head(tags$style(HTML(
    "
    #map-panel { height: calc(100% - 140px); }
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
      uiOutput('optimize'),
      uiOutput('school'),
      uiOutput('block')
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
    assignment_rev=0,
    selected_block='', selected_school='', selected_school_index=NULL,
    running_optimization=FALSE,
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
    r$selected_school_index = schools$spatial_name %>% detect_index(~ .x == r$selected_school)
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
      r$blocks[r$blocks$BLK == r$selected_block, 'school'] = ifelse(is.na(currently_assigned_school) | r$selected_school != currently_assigned_school, r$selected_school, '')
      r$blocks$updated = F
      r$blocks[r$blocks$BLK == r$selected_block, 'updated'] = T
      r$blocks[r$blocks$BLK == r$selected_block, 'selected'] = T
      r$assignment_rev = r$assignment_rev + 1
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
      r$blocks[!is.na(r$blocks$school) & r$blocks$school == selected_school, 'selected'] = T
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
  
  observe({
    if (r$running_optimization) {
      invalidateLater(0, session)
      isolate({
        # if there is no population to iterate on
        if (is.null(r$ga_population)) {
          # take the current blocks as a seed
          # select only blocks with stats and assign random schools for unassigned blocks
          seed_individual = blocks %>% as.data.frame() %>%
            filter(BLK %in% block_ids) %>%
            select(BLK, school) %>%
            mutate(school=sample(school_ids, nrow(.), replace = T))
            #mutate(school=ifelse(is.na(school), sample(school_ids, length(is.na(school)), replace = T), school))
          
          # prepend it to the current population
          r$ga_population = rep(list(seed_individual), 20) %>% map(ga_mutate)
        } else {
          # go just one evolutionary step
          r$ga_population = ga_step(r$ga_population, fitness_f,
                                    survival_fraction=0.5, mutation_fraction=0.1, mating_factor=1)
          
          fittest = r$ga_population[[1]]
          cat('Fitness ', fitness_f(fittest), '\n', file=stderr())
          cat('Polulation size ', length(r$ga_population), '\n', file=stderr())
          
          prev_schools = r$blocks$school
          new_schools = r$blocks %>% as.data.frame() %>% select(BLK) %>% left_join(fittest) %>% .$school
          r$blocks$school = new_schools
          diff = r$blocks$school != prev_schools
          r$blocks$updated = ifelse(is.na(diff), T, diff)
        }
      })
    }
  })
  

  # takes blocks spdf and returns new school assignments
  school_ids = as.character(schools$spatial_name)
  block_ids = unique(block_stats$BLK) # blocks with stats
  
  # An individual is an assignment data frame BLK->school
  # every gene is an assignment BLK->school
  
  # randomly reassign a number of schools
  ga_mutate = function(individual, fraction=0.01) {
    num_mutations = fraction*nrow(individual)
    individual[sample(1:nrow(individual), num_mutations), 'school'] = sample(school_ids, num_mutations, replace=T)
    individual
  }
  
  # randomly mix assignments from two individuals into one
  ga_crossover = function(a, b) {
    child = a[,]
    fraction = runif(1)
    num_genes_b = fraction*nrow(b)
    b_genes = sample(1:nrow(b), num_genes_b)
    child[b_genes, 'school'] = b[b_genes, 'school']
    child
  }
  
  # takes a list of individuals and breeds new ones in addition
  # returns the new population (that includes the old one)
  ga_breed = function(population, mutation_fraction=0.01, mating_factor=1) {
    mate_a = sample(population, length(population)*mating_factor, replace = T)
    mate_b = sample(population, length(population)*mating_factor, replace = T)
    
    c(population, map2(mate_a, mate_b, ~ ga_mutate(ga_crossover(.x, .y), mutation_fraction)))
  }
  
  # selects the fittest individuals according to a fittness function
  ga_select = function(population, fitness_f, survival_fraction=0.1, max_population=100) {
    fitness = map(population, fitness_f)
    num_survivors = min(ceiling(length(population)*survival_fraction), max_population)
    #survivors = population[rank(unlist(fitness), t = 'r') <= num_survivors]
    survivors = sort_by(population, fitness_f)[1:num_survivors]
    survivors
  }
  
  # takes a populations, runs beeds and selects
  ga_step = function(population, fittness_f, survival_fraction=0.5, mutation_fraction=0.05, mating_factor=1) {
    ga_select(ga_breed(population, mutation_fraction, mating_factor), fittness_f, survival_fraction)
  }
  
  ### 
  
  fitness_f = function(individual) {
    OVER_CAPACITY_PENALTY = 100
    individual %>% inner_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
      group_by(school) %>%
      summarise(kids=sum(kids), Kapa=first(Kapa), avg=sum(avg)) %>%
      rowwise() %>% mutate(over_capacity=max(0, kids-Kapa)) %>% ungroup %>%
      mutate(Kapa_penalty=over_capacity*OVER_CAPACITY_PENALTY) %>%
      summarise(avg=sum(avg), Kapa_penalty=sum(Kapa_penalty)) %>%
      mutate(fitness=avg+Kapa_penalty) %>% .$fitness
  }

  ### table

  reactive_table_data = reactive({
    r$assignment_rev
    data = isolate(r$blocks) %>% as.data.frame() %>%
      mutate(school=ifelse(is.na(school) | school == '', 'Keine', school))

    alternative = data
    if (r$selected_block != '') {
      currently_assigned_school = alternative[block_index[r$selected_block],]$school
      alternative[block_index[r$selected_block], 'school'] = ifelse(is.na(currently_assigned_school) | r$selected_school != currently_assigned_school, r$selected_school, '')
      alternative = alternative %>% mutate(school=ifelse(is.na(school) | school == '', 'Keine', school))
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
  
  output$table = renderDataTable({
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
      div(h4('TODO'))
    } else {
      div(h4(r$selected_school))
    }
  })

  output$block = renderUI({
    div(h4(r$selected_block))
  })
  
  output$optimize = renderUI({
    actionButton('optimize', label = ifelse(r$running_optimization, 'Stop optimization', 'Run optimization'))
  })

}

shinyApp(ui, server)
