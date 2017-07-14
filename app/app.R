source('deps.R')
source('ga.R')
source('io.R')
source('logging.R')
source('vars.R')
source('serv.R')
source('ui.R')

options(shiny.autoreload=T)
options(shiny.host="0.0.0.0")
options(warn=-1)

shinyApp(ui, server)