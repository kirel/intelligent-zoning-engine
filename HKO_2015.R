library(dplyr)
library(sp)
library(readr)
library(rgdal)

addresses = read_delim("data/HKO_2015_EPSG5650.txt", ';', locale = locale(encoding = "latin1")) %>%
  mutate(EEEEEEEE_EEE=1.0*EEEEEEEE_EEE/1000.0, NNNNNNN_NNN=1.0*NNNNNNN_NNN/1000.0) %>%
  select(OI, STN, HNR, PLZ, EEEEEEEE_EEE, NNNNNNN_NNN) %>%
  distinct(STN, HNR, PLZ) %>%
  as.data.frame()

coordinates(addresses) = c('EEEEEEEE_EEE', 'NNNNNNN_NNN')
proj4string(addresses) = CRS("+init=epsg:5650")

addresses = spTransform(addresses, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
row.names(addresses) = addresses$OI

writeOGR(addresses, "data/HKO_2015.geojson", layer="HKO_2015", driver="GeoJSON", check_exists = FALSE)