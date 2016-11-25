
library(shiny)

solution = read.table('idalab/intelligent-zoning-engine/zmq_plaground/python_stuff/data_file.csv')
settings = read.table('idalab/intelligent-zoning-engine/zmq_plaground/python_stuff/settings.csv')


ui <- fluidPage(
  actionButton(inputId = "change",
               label="change something"),
  uiOutput('opt', inline = TRUE),
  tableOutput('val')
)

server <- function(input, output, session) {
  r <- reactiveValues(
    running_optimization=FALSE,
    solution=runif(4,0,1))
  
  observeEvent(input$opt, {
    if (r$running_optimization) {
      write("stop", 'idalab/intelligent-zoning-engine/zmq_plaground/python_stuff/status_file.txt')
      r$running_optimization <- FALSE
    } else {
      write("start", 'idalab/intelligent-zoning-engine/zmq_plaground/python_stuff/status_file.txt')
      r$running_optimization <- TRUE
    }
  })
  
  observeEvent(input$change, {
    settings <- runif(4, 0,1)
    write.table(settings, 'idalab/intelligent-zoning-engine/zmq_plaground/python_stuff/settings.csv', sep=',', col.names=F, row.names=F)
    print(settings)
  })
  
  observe({
    if (r$running_optimization) {
      new_solution <- read.table('idalab/intelligent-zoning-engine/zmq_plaground/python_stuff/data_file.csv')
      if(!(all(new_solution == r$solution))){
        r$solution <- new_solution
      }
      invalidateLater(500, session)
    }
  })
  
  output$opt = renderUI({
    actionButton('opt', label = ifelse(r$running_optimization, 'Optimierung stoppen', 'Optimierung starten'))
  })
  
  output$val = renderTable({r$solution})
  
}

shinyApp(ui = ui, server = server)