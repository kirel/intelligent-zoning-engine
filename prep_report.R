# Dependencies ------------------------------------------------------------

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(formattable))
suppressPackageStartupMessages(library(webshot))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(rmarkdown))

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

assignment = units@data %>%
  dplyr::select(unit_id) %>%
  dplyr::left_join(read_csv(file.path(data_path, 'assignment.csv')), by = "unit_id") %>%
  dplyr::mutate(entity_id = ifelse(is.na(entity_id), NO_ASSIGNMENT, entity_id)) # FIXME necessary?

units = units %>% sp::merge(assignment)

# Map ---------------------------------------------------------------------

berlin <- ggmap::get_map("Berlin")

# Render Report -----------------------------------------------------------

rmarkdown::render(
  file.path("app", "templates", "assignment_report_de.Rmd"),
  params = list(
    map = berlin,
    units = units,
    entities = entities,
    assignment = assignment,
    NO_ASSIGNMENT = NO_ASSIGNMENT
  )
)
