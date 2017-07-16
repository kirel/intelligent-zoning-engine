options(googlesheets.webapp.client_id = "1010622408793-2ausgp88cdesnfcrd2jogtb5872b25ip.apps.googleusercontent.com",
        googlesheets.webapp.client_secret = "x38ERd6aD1TQgbVK-RAFx0qY",
        googlesheets.webapp.redirect_uri = "http://127.0.0.1:3838")
options(shiny.port=3838)

library(RColorBrewer)
library(leaflet)

NONE_SELECTED = '__NONE_SELECTED__'
NO_ASSIGNMENT = NONE_SELECTED

# Scales
# cfac = colorFactor(rainbow(length(entity_ids)), levels=sample(entity_ids))
colors = brewer.pal(8, 'Set1') # show_col(brewer.pal(9,'Set1'))
valid_colors = colors[2:8]
warning_color =  colors[1]
entity_ids_color = c(
  "07G16", "07G02", "07G01", "07G21", "07G14", "07G07", "07G15", "07G03", "07G13", "07G12", "07G10", "07G17", "07G06", "07G18",
  "07G19", "07G24", "07G05", "07G22", "07G23", "07G25", "07G20", "07G36", "07G27", "07G37", "07G35", "07G30", "07G28", "07G32",
  "07G29", "07G34", "07G26", "07G31"
)
palette = rep(valid_colors, length(entity_ids_color)/length(valid_colors)+1)[1:length(entity_ids_color)]
cfac = colorFactor(palette, levels=entity_ids_color)
entity_colors = function(entity_id, desaturate = FALSE) {
  color = cfac(entity_id)
  ifelse(desaturate, desat(color, 0.3), color)
}

desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}

# Colors for Report

color_vec = c(palette, warning_color)
names(color_vec) = c(entity_ids_color, NO_ASSIGNMENT)
