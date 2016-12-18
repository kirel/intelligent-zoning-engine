# Dependencies ------------------------------------------------------------

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(formattable))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(xtable))

# Globals -----------------------------------------------------------------

data_path <- file.path("app", "data")
NONE_SELECTED = '__NONE_SELECTED__'
NO_ASSIGNMENT = NONE_SELECTED

# Data Loading ------------------------------------------------------------

units = rgdal::readOGR(file.path(data_path, 'units.geojson'),
                layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
entities = rgdal::readOGR(file.path(data_path, 'entities.geojson'),
                          layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
weights = readr::read_csv(file.path(data_path, 'weights.csv'))

assign_path = file.path(data_path, 'assignment.csv')
# assign_path = '/home/moritz/Downloads/assignment_2016-12-13.csv'

assignment = units@data %>%
  dplyr::select(unit_id) %>%
  dplyr::left_join(read_csv(assign_path), by = "unit_id") %>%
  dplyr::mutate(entity_id = ifelse(is.na(entity_id), NO_ASSIGNMENT, entity_id)) # FIXME necessary?

units = units %>% sp::merge(assignment)
optimizable_units = units %>%
  as.data.frame() %>%
  inner_join(weights, by='unit_id') %>%
  .$unit_id %>%
  unique()

# Map ---------------------------------------------------------------------

map_path = file.path(data_path, 'berlin.rds')
if (file.exists(map_path)) {
  berlin = read_rds(map_path)
} else {
  berlin = ggmap::get_map('Berlin')
  write_rds(berlin, map_path, compress = 'gz')
}

# Colors ------------------------------------------------------------------

colors = RColorBrewer::brewer.pal(8, 'Set1') # show_col(brewer.pal(9,'Set1'))
valid_colors = colors[2:8]
warning_color =  colors[1]
entity_ids_color = c(
  "07G16", "07G02", "07G01", "07G21", "07G14", "07G07", "07G15", "07G03", "07G13", "07G12", "07G10", "07G17", "07G06", "07G18",
  "07G19", "07G24", "07G05", "07G22", "07G23", "07G25", "07G20", "07G36", "07G27", "07G37", "07G35", "07G30", "07G28", "07G32",
  "07G29", "07G34", "07G26", "07G31"
)
color_vec = rep(valid_colors, length(entity_ids_color) / length(valid_colors)+1)[1:length(entity_ids_color)]
color_vec = c(color_vec, warning_color)
names(color_vec) = c(entity_ids_color, NO_ASSIGNMENT)

# Render Report -----------------------------------------------------------

rmarkdown::render(
  file.path("app", "templates", "table.Rmd"),
  params = list(
    map = berlin,
    units = units,
    entities = entities,
    NO_ASSIGNMENT = NO_ASSIGNMENT,
    colors = color_vec,
    optimizable_units = optimizable_units,
    weights = weights
  ),
  envir = new.env(), clean = TRUE
)
