ui <- fillPage(
  shinyStore::initStore("store", "shinyStore-ize1"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel="shortcut icon", href="http://idalab.de/favicon.ico"),
    tags$title(HTML("idalab - intelligent zoning engine")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # colors for table and map
    tags$style(HTML(
      do.call(paste0, map2(entity_ids_color, cfac(entity_ids_color),
                           ~ paste0(
                             "#map .entity-color-", .x, " {color:", .y, " !important;}\n",
                             "#map .entity-bg-", .x, " {background-color:", .y, " !important;}\n",
                             "#map .entity-", .x, " {fill:", .y,";}\n",
                             "#map .unit.unit-assigned-to-entity-", .x, " {fill:", .y,";}\n"
                           )))
    ))
  ),
  # stripes pattern
  div(
    id='svg-patterns',
    HTML('<svg height="5" width="5" xmlns="http://www.w3.org/2000/svg" version="1.1"> <defs> <pattern id="locked-pattern" patternUnits="userSpaceOnUse" width="5" height="5"> <image xlink:href="data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1IiBoZWlnaHQ9IjUiPgo8cGF0aCBkPSJNMCA1TDUgMFpNNiA0TDQgNlpNLTEgMUwxIC0xWiIgc3Ryb2tlPSIjMDAwIiBzdHJva2Utd2lkdGg9IjEiPjwvcGF0aD4KPC9zdmc+" x="0" y="0" width="5" height="5"> </image> </pattern> </defs> </svg>'),
    HTML('<svg height="10" width="10" xmlns="http://www.w3.org/2000/svg" version="1.1"> <defs> <pattern id="diagonal-stripe-1" patternUnits="userSpaceOnUse" width="10" height="10"> <image xlink:href="data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxMCcgaGVpZ2h0PScxMCc+CiAgPHJlY3Qgd2lkdGg9JzEwJyBoZWlnaHQ9JzEwJyBmaWxsPSd3aGl0ZScvPgogIDxwYXRoIGQ9J00tMSwxIGwyLC0yCiAgICAgICAgICAgTTAsMTAgbDEwLC0xMAogICAgICAgICAgIE05LDExIGwyLC0yJyBzdHJva2U9J2JsYWNrJyBzdHJva2Utd2lkdGg9JzEnLz4KPC9zdmc+Cg==" x="0" y="0" width="10" height="10"> </image> </pattern> </defs> </svg>'),
    HTML('<svg height="8" width="8" xmlns="http://www.w3.org/2000/svg" version="1.1"> <defs> <pattern id="crosshatch" patternUnits="userSpaceOnUse" width="8" height="8"> <image xlink:href="data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPSc4JyBoZWlnaHQ9JzgnPgogIDxyZWN0IHdpZHRoPSc4JyBoZWlnaHQ9JzgnIGZpbGw9JyNmZmYnLz4KICA8cGF0aCBkPSdNMCAwTDggOFpNOCAwTDAgOFonIHN0cm9rZS13aWR0aD0nMC41JyBzdHJva2U9JyNhYWEnLz4KPC9zdmc+Cg==" x="0" y="0" width="8" height="8"> </image> </pattern> </defs> </svg>'),
    HTML('<svg version="1.1"
         baseProfile="full"
         xmlns="http://www.w3.org/2000/svg">
         <filter id="desaturate">
         <feColorMatrix id="matrix" type="matrix" values=".4  .3  .3   0   0
         .3  .4  .3   0   0
         .3  .3  .4   0   0
         0   0   0   1   0 "/>
         </filter>
         </svg>')
    ),
  fillRow(
    div(
      id='map-panel',
      leafletOutput("map", width="100%", height='100%'),
      div(id="map-controls",
          checkboxInput("show_utilization", "Auslastung anzeigen", value = TRUE),
          checkboxInput("show_population", "Population anzeigen", value = FALSE)
      ),
      tabsetPanel(type="tabs", id="tabs",
                  tabPanel("Details", div(id='detail',
                                          fillRow(
                                            div(id='detail--entity',
                                                h5('Schule:'),
                                                h4(id='detail--entity--selected-school', uiOutput('selected_entity')),
                                                div(id='detail--entity--controls',
                                                    actionButton('deselect_entity', '', icon=icon('remove'))
                                                ),
                                                tableOutput('selected_entity_table')
                                            ),
                                            div(id='detail--units',
                                                h5('Blöcke:'),
                                                h4(id='detail--units--selected-units', uiOutput('selected_units')),
                                                div(id='detail--units--controls',
                                                    actionButton('deselect_units', '', icon=icon('remove')),
                                                    actionButton('assign_units', '', icon=tags$i(class='icon-link-unit')),
                                                    actionButton('deassign_units', '', icon=tags$i(class='icon-unlink-unit')),
                                                    actionButton('lock_units', '', icon=icon('lock')),
                                                    actionButton('unlock_units', '', icon=icon('unlock'))
                                                ),
                                                tableOutput('selected_units_table')
                                            )
                                          )
                                          # TODO move buttons into UI outputs
                  )),
                  tabPanel("Import/Export", div(id='io',
                                                downloadButton('report', 'Report'),
                                                downloadButton('addresses', 'Adressliste'),
                                                downloadButton('serveAddresses', 'Adressliste als CSV'),
                                                downloadButton('serveGeoJSON', 'GeoJSON'),
                                                downloadButton('serveAssignment', 'Zuordnung Herunterladen'),
                                                fileInput('readAssignment', 'Zuordnung Hochladen',
                                                          accept = c('text/csv',
                                                                     'text/comma-separated-values',
                                                                     'text/plain',
                                                                     '.csv')
                                                ),
                                                actionButton('reset_assignment', 'Reset', icon=icon('fast-backward'))
                  )),
                  tabPanel("Optimierung", div(id='optimize-panel',
                                              uiOutput('optimize_button', inline = TRUE),
                                              plotOutput('fitness', height = '120px')
                  )),
                  tabPanel("Über das Projekt", div(id='about',
                                                   includeMarkdown("about.md")
                  ))
      )
    ),
    div(
      id='table-panel',
      DT::dataTableOutput("table")
    )
  ),
  singleton(
    tags$head(tags$script(src = "lodash.min.js"))
  ),
  singleton(
    tags$head(
      tags$script(src = "https://d3js.org/d3-array.v1.min.js"),
      tags$script(src = "https://d3js.org/d3-collection.v1.min.js"),
      tags$script(src = "https://d3js.org/d3-color.v1.min.js"),
      tags$script(src = "https://d3js.org/d3-format.v1.min.js"),
      tags$script(src = "https://d3js.org/d3-interpolate.v1.min.js"),
      tags$script(src = "https://d3js.org/d3-scale.v1.min.js")
    )
  ),
  singleton(
    tags$head(tags$script(src = "message-handler.js"))  
  )
)
