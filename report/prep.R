# Dependencies ------------------------------------------------------------

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(dplyr))

# Globals -----------------------------------------------------------------

root <- "~/Work/Idalab/intelligent-zoning-engine"
data_path <- file.path(root, "app", "data")

# Data Loading ------------------------------------------------------------

solution = readr::read_rds(file.path(data_path, "init_solution.rds")) %>%
  select(BLK, school) %>%
  mutate_all(as.character)

blocks = readr::read_rds(file.path(data_path, "blocks.rds")) %>%
  sp::merge(solution)

schools = read_rds(file.path(data_path, "schools.rds")) %>%
  mutate(spatial_name = as.character(spatial_name))

block_stats = readr::read_rds(file.path(data_path, "block_stats.rds"))

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

map_path <- file.path(data_path, "berlin.rds")
if (file.exists(map_path)) {
  berlin <- readr::read_rds(map_path)
} else {
  berlin <- ggmap::get_map("Berlin")
  readr::write_rds(berlin, map_path, compress = "gz")
}

# Render Report -----------------------------------------------------------

rmarkdown::render(file.path(root, "report", "zuordnung.Rmd"), params = list(
  map = berlin,
  blocks = blocks,
  schools = schools,
  school_stats = table_data,
  school_blocks = block_school
))
