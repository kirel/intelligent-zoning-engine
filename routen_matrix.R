library(dplyr)
library(tidyr)
library(rgdal)
library(readr)
library(osrm)
library(progress)
library(purrr)

options(osrm.server = "http://localhost:5000/")
options(osrm.profile = "foot")

umkreissuche = read_csv('data/umkreissuche.csv')

HKO_2015 = readOGR('data/HKO_2015.geojson', 'OGRGeoJSON')
HKO_2015_df = cbind(coordinates(HKO_2015), HKO_2015@data) %>% select(OI=OI, long=coords.x1, lat=coords.x2)

re_schulstand = readOGR('data/re_schulstand.geojson', 'OGRGeoJSON')
re_schulstand_df = cbind(coordinates(re_schulstand), re_schulstand@data) %>%
  select(school_id=spatial_name, s_long=coords.x1, s_lat=coords.x2) %>%
  filter(grepl('G', school_id))

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

slice_size = 10000
pb = progress_bar$new(total = nrow(HKO_2015_df) %/% slice_size, format = "[:bar] :current of :total / :percent / eta: :eta", clear = F)
HKO_2015_df %>%
  group_by(row_number() %/% slice_size) %>%
  by_slice(~ pb$tick() %after% retry_null(osrmTable(src = .x, dst = re_schulstand_df))$durations %>% as.data.frame %>% mutate(., src=rownames(.)) %>% gather(dst, time, -src), .collate="rows", .labels=F) %>%
  write_csv('data/routen_matrix.csv')