library(readr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggmap)
library(geosphere)
library(stplanr)
library(purrr)
library(lawn)
library(scales)
library(ggrepel)

addresses = read_delim("HKO_2015_EPSG5650.txt", ';', locale = locale(encoding = "latin1")) %>%
  mutate(EEEEEEEE_EEE=1.0*EEEEEEEE_EEE/1000.0, NNNNNNN_NNN=1.0*NNNNNNN_NNN/1000.0) %>%
  select(OI, STN, HNR, PLZ, EEEEEEEE_EEE, NNNNNNN_NNN) %>%
  as.data.frame()

coordinates(addresses) = c('EEEEEEEE_EEE', 'NNNNNNN_NNN')
proj4string(addresses) = CRS("+init=epsg:5650")

addresses = spTransform(addresses, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
row.names(addresses) = addresses$OI

bbox_addresses = bbox(addresses)

addresses_df = addresses %>% as.data.frame() %>% rename(long=EEEEEEEE_EEE, lat=NNNNNNN_NNN) %>% select(OI, STN, HNR, PLZ, lat, long)

schools = readOGR('schulen.geojson', layer = 'OGRGeoJSON') %>% as.data.frame() %>%
  rename(s_long=coords.x1, s_lat=coords.x2) %>% filter(SCHULART == 'Grundschule') %>%
  mutate(school_id=factor(gsub("G", "", spatial_name))) %>%
  select(school_id, s_lat, s_long, SCHULNAME)

assignment_areas = readOGR('ESB2012_WGS84_EWR2012-12/ESB2012_WGS84_EWR2012-12.shp', layer='ESB2012_WGS84_EWR2012-12')
assignment_areas = spTransform(assignment_areas, CRS(proj4string(addresses)))
assignment_areas = assignment_areas[,c("ID")]
row.names(assignment_areas) = as.character(assignment_areas$ID)
plot(assignment_areas)

assigment_area_centroids = coordinates(assignment_areas) %>% as.data.frame() %>% rename(long=V1, lat=V2)
ggplot() +
  coord_map() +
  theme_void() +
  guides(color=F)


school_assignments = over(addresses, assignment_areas, returnList = T)
length(map(school_assignments, nrow) %>% keep(~ . > 1)) # => 0

school_assignments = over(addresses, assignment_areas)

addresses_df = cbind(addresses_df, school_assignments) %>% mutate(school_id=factor(as.character(ID), levels=schools$school_id))

ways = inner_join(addresses_df, schools, by=c("school_id")) %>%
  rowwise() %>%
  mutate(air_dist=distHaversine(c(long, lat), c(s_long, s_lat)), way_id=paste(OI, school_id)) %>%
  ungroup() %>% as.data.frame()

assignment_areas_df = fortify(assignment_areas)

ggplot() +
  geom_polygon(aes(long, lat, group=group, order=order), color='red', fill='white', data = assignment_areas_df) +
  geom_text(aes(long, lat, label=ID), size=2, color='red', data=cbind(assigment_area_centroids, assignment_areas@data)) +
  #geom_segment(aes(long, lat, xend=s_long, yend=s_lat, color=air_dist, order=air_dist),
  #             size=0.1, data=ways %>% sample_n(10000)) +
  geom_text(aes(s_long, s_lat, label=school_id), size=2, color='black', data=schools) +
  scale_color_continuous(low='blue', high='red') +
  coord_map() +
  theme_void() +
  guides(color=F)

# http://gis.stackexchange.com/questions/139681/overlaying-lines-and-aggregating-their-values-for-overlapping-segments
# graphhopper http://www.inside-r.org/node/349079
# http://www.inside-r.org/packages/cran/rgeos/docs/gIntersection

graphhopper = function (from, to, vehicle = "foot", silent = TRUE) 
{
  if (is.character(from) | is.character(to)) {
    from <- rev(RgoogleMaps::getGeoCode(from))
    to <- rev(RgoogleMaps::getGeoCode(to))
  }
  api_base <- "http://localhost:8990/route?"
  orig <- paste0(from[2:1], collapse = "%2C")
  dest <- paste0(to[2:1], collapse = "%2C")
  ft_string <- paste0("point=", orig, "&point=", dest)
  veh <- paste0("&vehicle=", vehicle)
  request <- paste0(api_base, ft_string, veh, "&locale=en-US&debug=true&points_encoded=false")
  if (vehicle == "bike") {
    request <- paste0(request, "&elevation=true")
  }
  if (silent == FALSE) {
    print(paste0("The request sent was: ", request))
  }
  txt <- httr::content(httr::GET(request), as = "text")
  obj <- jsonlite::fromJSON(txt)
  #browser()
  coords = obj$paths$points$coordinates[[1]][, 1:2]
  route <- sp::SpatialLines(list(sp::Lines(list(sp::Line(coords)), ID = "1")))
  climb <- NA
  if (vehicle == "bike") {
    elevs <- obj$paths$points[[1]][[1]][, 3]
    climb <- elevs[-1] - elevs[-length(elevs)]
    climb <- sum(climb[climb > 0])
  }
  df <- data.frame(time = obj$paths$time/(1000 * 60), dist = obj$paths$distance, 
                   climb = climb)
  route <- sp::SpatialLinesDataFrame(route, df)
  proj4string(route) <- CRS("+init=epsg:4326")
  route
}

plot(graphhopper(c(13.347015,52.536273), c(13.452759,52.545461)))

to_gh_result = function(row) {
  tryCatch(spChFIDs(graphhopper(as.numeric(c(row[['long']],row[['lat']])), as.numeric(c(row[['s_long']],row[['s_lat']]))), row[['way_id']]), error=function(e) NULL)
}

gh_results = select(ways, long, lat, s_long, s_lat, way_id) %>% apply(1, to_gh_result)
gh_results = gh_results %>% purrr::discard(is_null)

routed_ways = do.call(rbind, gh_results)
routed_ways %>% saveRDS('routed_ways.rds')
rm(gh_results)
routed_ways = readRDS('routed_ways.rds')

plot(routed_ways)

routed_ways_data = routed_ways@data %>% mutate(id=rownames(routed_ways@data))
routed_ways_df = fortify(routed_ways) %>% inner_join(routed_ways_data) %>% inner_join(ways %>% rename(h_long=long, h_lat=lat), by=c("id"="way_id"))

# box = bbox(routed_ways)
# box[,'max'] = box[,'max'] + 0.1*(box[,'max'] - box[,'min'])
# box[,'min'] = box[,'min'] - 0.1*(box[,'max'] - box[,'min'])

# map = get_map(box, zoom=10, maptype = 'toner-lite')
# 
# ggmap(map) +
#   geom_path(aes(long, lat, group=group, color=school_id), size=0.1, data=routed_ways_df) +
#   geom_point(aes(s_long, s_lat, color=school_id), data=schools %>% filter(school_id %in% routed_ways_df$school_id)) +
#   scale_color_manual(values=rep(rainbow(10), 100), guide=F) +
#   guides(color=F)
#   
# ggmap(map) +
#   geom_path(aes(long, lat, group=group, color=time), size=0.1, data=routed_ways_df %>% arrange(time, group, order)) +
#   geom_point(aes(s_long, s_lat), data=schools %>% filter(school_id %in% routed_ways_df$school_id)) +
#   scale_color_continuous(low='green', high='red') +
#   guides(color=F)

max_dist = max(routed_ways$dist)

random_school = ways$school_id %>% unique %>% sample(1)
data = routed_ways_df %>% filter(school_id %in% random_school) %>% arrange(desc(time))
data = data %>% mutate(group=as.character(group)) %>% mutate(group=match(group, unique(group)))

ggplot() +
  geom_path(aes(long, lat, group=group, order=dist, color=dist, size=time), data=data) +
  geom_point(aes(s_long, s_lat), size=0.5, data=schools %>% filter(school_id == random_school)) +
  scale_color_gradientn(colors=c('green', 'orange', 'red', 'red'), values=rescale(c(0,1900,2000,max_dist)), limits=c(0, max_dist)) +
  scale_size_continuous(range=c(1,0.001)) +
  coord_map() +
  theme_void() +
  ggtitle(paste("Einzugsgebiet", filter(schools, school_id == random_school)$SCHULNAME)) +
  guides(color=F, size=F)

ggsave('schulen.png', dpi=600)

box = bbox(data[c('long', 'lat')] %>% as.matrix())
map = get_map(box, source='stamen', maptype = 'toner-lite')

ggmap(map) +
  geom_path(aes(long, lat, group=group, order=dist, color=dist, size=time), data=data) +
  geom_point(aes(s_long, s_lat), size=0.5, data=schools %>% filter(school_id == random_school)) +
  scale_color_gradientn(colors=c('green', 'orange', 'red', 'red'), values=rescale(c(0,1900,2000,max_dist)), limits=c(0, max_dist)) +
  scale_size_continuous(range=c(1,0.001)) +
  coord_map() +
  theme_void() +
  ggtitle(paste("Einzugsgebiet", filter(schools, school_id == random_school)$SCHULNAME)) +
  guides(color=F, size=F)


### try to not sort but clip overlapping ways

row.names(ways) = ways$way_id
ways_in_routed = ways_df[match(row.names(routed_ways), row.names(ways)),]
routed_ways_xt = maptools::spCbind(routed_ways, ways_in_routed)


routed_ways_school = routed_ways_xt[routed_ways_xt$school_id == random_school,]
plot(routed_ways_school)
routed_ways_school = routed_ways_school[order(routed_ways_school$dist),]

#for (i in 2:length(routed_ways_school)) {
keep = list(routed_ways_school[1,])
for (i in 2:length(routed_ways_school)) {
  rest_of_line = rgeos::gDifference(routed_ways_school[i,], routed_ways_school[1:i-1,], id=row.names(routed_ways_school[i,]))
  if (!is.null(rest_of_line)) {
    rest_of_line = SpatialLinesDataFrame(rest_of_line, routed_ways_school[i,]@data)
    keep = c(keep, rest_of_line)
  }
}

deoverlapped_routed_ways_school = do.call(rbind, keep)

plot(deoverlapped_routed_ways_school)

save.image()
# coord_map
