library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(formattable)
library(htmltools)
library(purrr)
library(colorspace)
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
blocks$highlighted = F # on mouseover
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
ui <- fluidPage(
  leafletOutput("map"),
  inputPanel(
    selectizeInput("selected_block", "Block", c('', block_ids)),
    selectizeInput("selected_school", "Schule", c('', school_ids)),
    actionButton("assign", "Zuordnen")
  ),
  formattableOutput("table")
)

### Server
server <- function(input, output, session) {
  
  ### Reactive Values
  
  r <- reactiveValues(blocks=blocks, highlights=blocks[,], schools=schools, assignment_rev=0)
  
  reactive_schools = reactive(r$schools)

  reactive_selected_schools = reactive(r$schools %>% filter(selected=T))
  
  ### Interaction
  
  # block mouseover -> highlight the shape
  observeEvent(input$map_shape_mouseover, {
    req(input$map_shape_mouseover$id)
    if (grepl('block_', input$map_shape_mouseover$id)) {
      # select the respective block
      selected_block = sub('block_', '', input$map_shape_mouseover$id)
      updateSelectInput(session, "selected_block", selected=selected_block)
    }
  })
  
  # click on markers -> select school and blocks
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$id)
    clicked_marker = input$map_marker_click$id
    selected_school = ifelse(clicked_marker == isolate(input$selected_school), '', clicked_marker)
    updateSelectInput(session, "selected_school", selected=selected_school)
    updateSelectInput(session, "selected_block", selected='')
  })
  
  # click on shapes -> update the solution
  observeEvent(input$map_shape_click, {
    if (grepl('block_', input$map_shape_click$id)) {
      # a block was clicked
      clicked_block = sub('block_', '', input$map_shape_click$id)
      updateSelectInput(session, "selected_block", selected=clicked_block)
      # update the solution
      currently_assigned_school = r$blocks[r$blocks$BLK == input$selected_block,]$school
      r$blocks[r$blocks$BLK == input$selected_block, 'school'] = ifelse(is.na(currently_assigned_school) | input$selected_school != currently_assigned_school, input$selected_school, '')
      r$blocks$updated = F
      r$blocks[r$blocks$BLK == input$selected_block, 'updated'] = T
      r$blocks[r$blocks$BLK == input$selected_block, 'selected'] = T
      r$assignment_rev = r$assignment_rev + 1
    }
  })
  
  # click on button
  observeEvent(input$assign, {
    r$blocks[r$blocks$BLK == input$selected_block, 'school'] = input$selected_school
    r$blocks$updated = F
    r$blocks[r$blocks$BLK == input$selected_block, 'updated'] = T
    r$blocks[r$blocks$BLK == input$selected_block, 'selected'] = T
    r$assignment_rev = r$assignment_rev + 1
  })
  
  # select block in select input
  observe({
    selected_block = input$selected_block
    isolate({
      r$highlights$highlighted = F
      r$highlights[r$highlights$BLK == selected_block, 'highlighted'] = T
    })
  })
  
  # select school in select input
  observe({
    selected_school = input$selected_school
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
    highlighted_block = r$blocks[r$highlights$highlighted==T,]
    if (nrow(highlighted_block) == 1) {
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
    data = isolate(r$blocks)
    if (input$selected_block != '' & input$selected_school != '') {
      data = data[,] 
      data[data$BLK == input$selected_block, 'school'] = input$selected_school
    } else if (input$selected_block != '') {
      data = data[,] 
      data[data$BLK == input$selected_block, 'school'] = ''
    }
    data[is.na(data$school) | data$school == '', 'school'] = 'Keine'
    data %>% as.data.frame() %>%
      left_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
      left_join(kids_in_blocks, by='BLK') %>%
      group_by(school) %>% summarise(
        kids=sum(num_kids, na.rm=T),
        num_blocks=n(),
        min_time=min(min),
        avg_time=mean((kids*avg)/sum(kids)),
        max_time=max(max),
        Kapa=first(Kapa)
      ) %>%
      mutate(
        utilization=kids/Kapa
      ) %>% select(
        Schule=school,
        `Blöcke`=num_blocks,
        Kapazität=Kapa,
        Kinder=kids,
        Auslastung=utilization,
        `Weg (min)`=min_time,
        `Weg (Ø)`=avg_time,
        `Weg (max)`=max_time
      )
  })
  
  output$table = renderFormattable({
    reactive_table_data() %>%
      formattable(
        list(
          Kinder = formatter("span", x ~ digits(x, 2)),
          Auslastung = formatter("span",
                                 style = x ~ style(color = ifelse(x < 1, "green", "red")),
                                 x ~ icontext(ifelse(x < 1, "ok", "remove"), percent(x))
          ),
          `Weg (Ø)` = proportion_bar("lightblue", na.rm = T),
          `Weg (min)` = proportion_bar("lightblue", na.rm = T),
          `Weg (max)` = proportion_bar("lightblue", na.rm = T)
        )
      )
  })

}

shinyApp(ui, server)

