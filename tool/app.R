library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(formattable)
library(htmltools)
library(purrr)
library(colorspace)
library(DT)
options(shiny.autoreload=T)
library(shiny)
library(shinydashboard)

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
    "))),
  fillRow(
    div(
      id='map-panel',
      leafletOutput("map", width="100%", height='100%'),
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
  
  r <- reactiveValues(blocks=blocks, schools=schools, assignment_rev=0, selected_block='', selected_school='', previous_mouseover='')
  
  ### Interaction
  
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
        `Delta Ausl.`=delta_utilization,
        `Weg (min)`=min_time,
        `Weg (avg)`=avg_time,
        `Weg (max)`=max_time
      )
    
  })
  
  output$table = DT::renderDataTable({
    reactive_table_data() %>%
      formattable(
        list(
          Kinder = formatter("span", x ~ digits(x, 2)),
          Auslastung = formatter(
            "span",
            style = x ~ style(color = ifelse(x < 1, "green", "red")),
            x ~ icontext(ifelse(x < 1, "ok", "remove"), percent(x))
          ),
          `Delta Ausl.` = formatter(
            "span",
            x ~ icontext(ifelse(x == 0, "arrow-right", ifelse(x < 1, "arrow-down", "arrow-up")), percent(x))
          ),
          `Weg (avg)` = proportion_bar("lightblue", na.rm = T),
          `Weg (min)` = proportion_bar("lightblue", na.rm = T),
          `Weg (max)` = proportion_bar("lightblue", na.rm = T)
        )
      ) %>% as.datatable(
        options=c(list(paging = F, searching = F, stateSave = T, columnDefs=list(list(targets=c(2,3,5,6,7), class="dt-right"))), isolate(input$table_state)),
        selection=list(mode = 'single', selected = r$selected_school_index, target = 'row')
      )
  }, server = F)
  
  # Maybe via row callbacks? https://rstudio.github.io/DT/options.html
  
  # TODO dataTableProxy() selectRows() replaceData() https://rstudio.github.io/DT/shiny.html
  
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
  
}

shinyApp(ui, server)

