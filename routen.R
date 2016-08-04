library(dplyr)
library(rgdal)
library(readr)
library(osrm)
library(progress)

options(osrm.server = "http://localhost:5000/")
options(osrm.profile = "foot")

umkreissuche = read_csv('data/umkreissuche.csv')

HKO_2015 = readOGR('data/HKO_2015.geojson', 'OGRGeoJSON')
HKO_2015_df = cbind(coordinates(HKO_2015), HKO_2015@data) %>% rename(long=coords.x1, lat=coords.x2)

re_schulstand = readOGR('data/re_schulstand.geojson', 'OGRGeoJSON')
re_schulstand_df = cbind(coordinates(re_schulstand), re_schulstand@data) %>% rename(s_long=coords.x1, s_lat=coords.x2)

#umkreissuche = umkreissuche %>% filter(school_id == umkreissuche %>% sample_n(1) %>% .$school_id)

ways = inner_join(umkreissuche, HKO_2015_df, by=c("STN", "HNR", "PLZ")) %>%
  inner_join(re_schulstand_df, by=c("school_id"="spatial_name")) %>%
  distinct(OI, school_id)

pb = progress_bar$new(total = nrow(ways), format = "[:bar] :current of :total / :percent / eta: :eta")
routed_ways = ways %>% apply(1, function(row) {
  pb$tick()
  osrmRoute(row[c('OI', 'long', 'lat')], row[c('school_id', 's_long', 's_lat')], sp=TRUE)
}) %>% do.call(rbind, .)

writeOGR(routed_ways, "data/routen.geojson", layer="OGRGeoJSON", driver="GeoJSON", check_exists = FALSE)