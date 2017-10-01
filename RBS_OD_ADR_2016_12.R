library(dplyr)
library(sp)
library(readr)
library(rgdal)
library(stringr)

addresses = read_delim('download/RBS_OD_ADR_2016_12.csv', delim=';', locale=locale(decimal_mark = ",", grouping_mark = '')) %>%
  select(STRNR, STRNAME, HSNR, PLZ, BLK, BEZ, X_25833, Y_25833) %>%
  distinct(STRNR, HSNR, PLZ, .keep_all = TRUE) %>%
  mutate(nr=as.integer(str_extract(HSNR, '\\d+')), add=str_extract(HSNR, '[a-zA-Z]')) %>%
  as.data.frame()

coordinates(addresses) = c('X_25833', 'Y_25833')
proj4string(addresses) = CRS("+init=epsg:25833")

addresses = spTransform(addresses, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
row.names(addresses) = paste0(addresses$STRNR, addresses$HSNR)

writeOGR(addresses, "output/RBS_OD_ADR_2016_12.geojson", layer="RBS_OD_ADR_2016_12", driver="GeoJSON", check_exists = FALSE)