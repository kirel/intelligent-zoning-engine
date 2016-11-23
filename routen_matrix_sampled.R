library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(maptools)
library(readr)
library(progress)
library(purrr)
library(osrm)

options(osrm.server = "http://localhost:5000/")
options(osrm.profile = "foot")

#library(ggplot2)
#library(broom)

re_schulstand = readOGR('download/re_schulstand.geojson', 'OGRGeoJSON')
re_schulstand_df = cbind(coordinates(re_schulstand), re_schulstand@data) %>%
  select(school_id=spatial_name, s_long=coords.x1, s_lat=coords.x2) %>%
  filter(grepl('G', school_id))

sampled_buildings = read_csv('output/sampled_buildings.csv')


`%after%` = function(a , b) {
  b
  a
  b
}

# necessary because osrm doesn't give me errors
retry_null <- function(expr, silent=FALSE, max_attempts=10, verbose=FALSE) {
  expr = substitute(expr)
  for (attempt_i in seq_len(max_attempts)) {
    res = try(expr = eval(expr, parent.frame()), silent = silent)
    if (class(res) == "try-error" || is.null(res)) {
      if (verbose) {
        message("Failed attempt ", attempt_i)
      }
    } else {
      if (verbose) {
        message("Succeeded after ", attempt_i, " attempts.")
      }
      return(res)
    }
  }
  stop("Failed after ", max_attempts, results)
}

slice_size = 1000
pb = progress_bar$new(total = nrow(sampled_buildings) %/% slice_size, format = "[:bar] :current of :total / :percent / eta: :eta", clear = F)
routen_matrix = sampled_buildings %>% select(OI, long, lat) %>%
  group_by(row_number() %/% slice_size) %>%
  by_slice(~ pb$tick() %after% retry_null({
    table = osrmTable(src = .x, dst = re_schulstand_df)
    if (!is.null(table)) {
      durations = table$durations %>% as.data.frame %>% mutate(., src=rownames(.)) %>% gather(dst, duration, -src)
      distances = table$distances %>% as.data.frame %>% mutate(., src=rownames(.)) %>% gather(dst, distance, -src)
      inner_join(durations, distances, by=c('src', 'dst'))
    }
    }), .collate="rows", .labels=F)
routen_matrix_1000=routen_matrix

routen_matrix %>% write_csv('output/routen_matrix.csv')