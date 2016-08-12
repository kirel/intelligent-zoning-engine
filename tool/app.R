library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(formattable)
library(htmltools)
library(purrr)
library(shiny)

solution = read_rds('init_solution.rds')
blocks = read_rds('blocks.rds')
bez = read_rds('bez.rds')
schools = read_rds('schools.rds')
block_stats = read_rds('block_stats.rds')
school_ids = unique(as.character(schools$spatial_name))
block_ids = unique(as.character(blocks$BLK))

leaflet_schools = schools
school_colors = colorFactor(rainbow(length(school_ids)), levels=sample(school_ids))

ui <- fluidPage(
  leafletOutput("map"),
  selectizeInput("selected_block", "Block", c('', block_ids)),
  selectizeInput("selected_school", "Schule", c('', school_ids)),
  actionButton("assign", "Zuordnen"),
  formattableOutput("table")
)

server <- function(input, output, session) {
  
  rValues <- reactiveValues(button=FALSE, click=FALSE, no_clicks=0)
  
  calculate_updated_blocks = function(selected_block, selected_school) {
    if (selected_block == '') return(head(solution,0))
    else if (selected_school == '') {
      # unassign this block
      idx = solution$BLK %>% detect_index(~ .x == selected_block)
      solution[idx, 2] = selected_school
      solution <<- solution # FIXME necessary?
      return(solution[idx,])
    }
    else {
      # (re-)assign this block to this school
      idx = solution$BLK %>% detect_index(~ .x == selected_block)
      solution[idx, 2] = selected_school
      solution <<- solution # FIXME necessary?
      return(solution[idx,])
    }
  }
  
  event = observeEvent(input$assign, {
    isolate({
      rValues$button = TRUE
      rValues$click = FALSE
      rValues$clicked_on = NULL
    })
  })
  
  updated_solution = reactive({
    click = rValues$click
    rValues$no_clicks
    button = rValues$button
    isolate({
      selected_block = input$selected_block
      selected_school = input$selected_school
      if (click) {
        if (rValues$clicked_on == selected_block) {
          calculate_updated_blocks(selected_block, selected_school)
        }
        else return(head(solution,0)) 
      }
      else calculate_updated_blocks(selected_block, selected_school)
    })
  })
  
  initial_blocks = blocks %>% sp::merge(solution %>% inner_join(block_stats, by=c('BLK'='BLK', 'school'='dst')))
  
  leaflet_updated_blocks = reactive({
    blocks %>% sp::merge(updated_solution() %>% inner_join(block_stats, by=c('BLK'='BLK', 'school'='dst')), all.x = FALSE)
  })
  
  output$map <- renderLeaflet({
  
    m <- leaflet() %>%
      addProviderTiles("Stamen.Toner", option=providerTileOptions(opacity=0.2)) %>%  # Add default OpenStreetMap map tiles
      addPolylines(color='black', weight=4, opacity=1, data=bez) %>%
      addPolygons(data=initial_blocks, group='blocks', stroke = F, fillOpacity = 1, smoothFactor = 0.2, layerId= ~BLK,
                  color= ~school_colors(school)) %>% #, popup = ~htmlEscape(paste(BLK,'->',school,SCHULNAME))) %>%
      addCircleMarkers(data=leaflet_schools, group='schools', fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, layerId= ~spatial_name,
                       fillColor= ~school_colors(spatial_name), popup = ~htmlEscape(paste(spatial_name, SCHULNAME)))
    m
    
  })
  
  observe({
    data = leaflet_updated_blocks()
    if (is.null(data)) return()
    leafletProxy("map", data = data) %>%
      addPolygons(stroke = F, group='blocks', fillOpacity = 1, smoothFactor = 0.2, layerId= ~BLK,
                  color= ~school_colors(school), popup = ~htmlEscape(paste(BLK,'->',school,SCHULNAME))) %>%
      addCircleMarkers(data=leaflet_schools, group='schools', fillOpacity = 1, color='black', opacity=1, weight=2, radius=5, layerId= ~spatial_name,
                       fillColor= ~school_colors(spatial_name), popup = ~htmlEscape(paste(spatial_name, SCHULNAME)), options=markerOptions(zIndexOffset=100))
  })
  
  table_data = reactive({
    updated_solution()
    solution %>% inner_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
      group_by(school) %>% summarise(
        kids=sum(kids),
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
    table_data() %>%
      formattable(
        list(
          Kinder = formatter("span", x ~ digits(x, 2)),
          Auslastung = formatter("span",
                                 style = x ~ style(color = ifelse(x < 1, "green", "red")),
                                 x ~ icontext(ifelse(x < 1, "ok", "remove"), percent(x))
          ),
          `Weg (Ø)` = proportion_bar("lightblue"),
          `Weg (min)` = proportion_bar("lightblue"),
          `Weg (max)` = proportion_bar("lightblue")
        )
      )
  })
  
  observe({
    event <- input$map_shape_click
    if (is.null(event)) return()
    isolate({
      rValues$button = FALSE
      rValues$click = TRUE
      rValues$no_clicks = rValues$no_clicks + 1
      rValues$clicked_on = event$id
    })
    updateSelectInput(session, "selected_block", selected=event$id)
  })
  
  observe({
    event <- input$map_marker_click
    if (is.null(event)) return()
    updateSelectInput(session, "selected_school", selected=event$id)
    updateSelectInput(session, "selected_block", selected='')
  })
  
}

shinyApp(ui, server)

