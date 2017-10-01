library(dplyr)
library(sp)
library(readr)
library(rgdal)
library(stringr)

addresses = read_delim("download/HKO_2015_EPSG5650.txt", ';', locale = locale(decimal_mark = ",", grouping_mark = '', encoding = "latin1")) %>%
  select(OI, STN, HNR, PLZ, EEEEEEEE_EEE, NNNNNNN_NNN) %>%
  distinct(STN, HNR, PLZ, .keep_all = TRUE) %>%
  as.data.frame()

coordinates(addresses) = c('EEEEEEEE_EEE', 'NNNNNNN_NNN')
proj4string(addresses) = CRS("+init=epsg:5650")

addresses = spTransform(addresses, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
row.names(addresses) = addresses$OI

writeOGR(addresses, "output/HKO_2015.geojson", layer="HKO_2015", driver="GeoJSON", check_exists = FALSE)