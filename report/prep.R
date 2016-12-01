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

root <- "/home/moritz/Work/Idalab/intelligent-zoning-engine"
data_path <- file.path(root, "app", "data")
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
  dplyr::left_join(read_csv(file.path(data_path, 'assignment.csv'))) %>%
  dplyr::mutate(entity_id = ifelse(is.na(entity_id), NO_ASSIGNMENT, entity_id)) # FIXME necessary?

units = units %>% sp::merge(assignment)

bez = readOGR(file.path(data_path, 'RBS_OD_BEZ_2015_12.geojson'),
              layer = 'OGRGeoJSON',
              stringsAsFactors = FALSE) %>%
  subset(BEZ == '07')

# Data Wrangling ----------------------------------------------------------

data = units %>%
  as.data.frame() %>%
  mutate(entity_id = ifelse(entity_id == NO_ASSIGNMENT, 'Keine', entity_id))

table_data = data %>%
  dplyr::left_join(weights) %>%
  dplyr::group_by(entity_id) %>%
  dplyr::summarise(
    num_units=n(),
    min_dist=min(min, na.rm=T),
    avg_dist=mean((population*avg)/sum(population, na.rm=T), na.rm=T),
    max_dist=max(max, na.rm=T),
    pop=sum(population, na.rm=T)
  ) %>%
  dplyr::left_join(entities@data %>% select(entity_id, capacity)) %>%
  dplyr::mutate(utilization = pop / capacity)

# Map ---------------------------------------------------------------------

berlin <- ggmap::get_map("Berlin")

# Render Report -----------------------------------------------------------

rmarkdown::render(file.path(root, "report", "zuordnung.Rmd"), params = list(
  map = berlin,
  units = units,
  entities = entities,
  entity_stats = table_data,
  assignment = assignment
))
