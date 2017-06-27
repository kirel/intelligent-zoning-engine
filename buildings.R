library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(maptools)
library(readr)
library(progress)
library(purrr)

#library(ggplot2)
#library(broom)

HKO_2015 = readOGR('output/HKO_2015.geojson', 'OGRGeoJSON', stringsAsFactors = FALSE)
blk = spTransform(readOGR('download/RBS_OD_BLK_2015_12.geojson', 'OGRGeoJSON', stringsAsFactors = FALSE), CRS(proj4string(HKO_2015)))

buildings_df = HKO_2015 %>% as.data.frame() %>%
  mutate(long=coords.x1, lat=coords.x2) %>%
  cbind(HKO_2015 %>% over(blk))

print(buildings_df %>% filter(is.na(BLK)))
print(buildings_df %>% filter(Einw==0))

buildings_df %>% select(OI, long, lat, BLK) %>% write_rds('output/sampled_buildings.rds', 'gz')
