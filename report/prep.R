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

root <- "/home/shwifty/Work/Idalab/intelligent-zoning-engine"
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

bez = readOGR(file.path(data_path, 'RBS_OD_BEZ_2015_12.geojson'),
              layer = 'OGRGeoJSON',
              stringsAsFactors = FALSE) %>%
  subset(BEZ == '07')

# Data Wrangling ----------------------------------------------------------

kids_in_blocks = block_stats %>%
  dplyr::group_by(BLK) %>%
  dplyr::summarise(num_kids=dplyr::first(kids))

data = blocks %>%
  as.data.frame() %>%
  dplyr::mutate(school=ifelse(school == '', 'Keine', school))

table_data = data %>%
  dplyr::left_join(block_stats, by=c('BLK'='BLK', 'school'='dst')) %>%
  dplyr::left_join(kids_in_blocks, by='BLK') %>%
  dplyr::group_by(school) %>%
  dplyr::summarise(
    kids=sum(num_kids, na.rm=TRUE),
    num_blocks=n(),
    min_time=min(min, na.rm=TRUE),
    avg_time=mean((kids*avg) / sum(kids, na.rm=TRUE), na.rm=TRUE),
    max_time=max(max, na.rm=TRUE),
    Kapa=dplyr::first(Kapa)
  ) %>%
  dplyr::mutate(utilization=kids/Kapa) %>%
  dplyr::select(
    Schule=school,
    Kapazität=Kapa,
    Kinder=kids,
    Auslastung=utilization,
    `Weg (min)`=min_time,
    `Weg (Ø)`=avg_time,
    `Weg (max)`=max_time
  )

block_school <- solution %>%
  dplyr::group_by(school) %>%
  dplyr::summarise(blocks = paste(BLK, sep = "", collapse = ", "))

# Map ---------------------------------------------------------------------

berlin <- ggmap::get_map("Berlin")

# Render Report -----------------------------------------------------------

rmarkdown::render(file.path(root, "report", "zuordnung.Rmd"), params = list(
  map = berlin,
  blocks = units,
  schools = entities,
  school_stats = table_data,
  school_blocks = assignment
))
