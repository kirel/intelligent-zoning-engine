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

HKO_2015 = readOGR('data/HKO_2015.geojson', 'OGRGeoJSON')
re_schulstand = readOGR('data/re_schulstand.geojson', 'OGRGeoJSON')
blk = readOGR('data/RBS_OD_BLK_2015_12.geojson', 'OGRGeoJSON')
alkis = readOGR('data/re_alkis_tatsaechlichenutzungflaechen.geojson', layer = 'OGRGeoJSON')

blk$area = sapply(slot(blk, "polygons"), slot, "area")
residential = alkis[alkis$AAA.Beschreibung == 'AX_Wohnbauflaeche' | grepl('Wohnen', alkis$Funktion_bezeichnung),]

residential_buildings = HKO_2015[!is.na(over(HKO_2015, residential)$AAA.Beschreibung),]

residential_buildings_df = residential_buildings %>% as.data.frame() %>%
  select(OI, long=coords.x1, lat=coords.x2) %>%
  cbind(residential_buildings %>% over(blk)) %>%
  filter(!is.na(BLK)) %>% filter(Einw > 0)


samplesizes_n_stratified = function(src, size, strata, weights=src %>% group_by_(.dots=strata) %>% mutate(weights=1)) {
  if (size < nrow(src %>% group_by_(.dots=strata) %>% summarise())) stop('Too many strata')
  if (size > nrow(src)) stop('Not enough data')
  
  # first sample one from each group
  samplesizes = src %>% group_by_(.dots=strata) %>% summarise(n_=n()-1) %>%
    inner_join(weights, by=strata) %>% mutate(chosen=1, selectable=n_>0, weights=weights/sum(weights))
  nrow_samplesizes = nrow(samplesizes)
  samplesizes_idx = 1:nrow_samplesizes
  num_chosen = nrow_samplesizes
  pb <- progress_bar$new(total = size-nrow(samplesizes))
  while (num_chosen < size) {
    # check the weights and sample from biggest difference to weight
    #samplesizes = samplesizes %>% mutate(diff=chosen/sum(chosen)-weights) %>%
    #  arrange(-selectable, diff) %>% mutate(n_=n_-inc, chosen=chosen+inc, selectable=n_>0)
    samplesizes$diff = samplesizes$chosen/num_chosen-samplesizes$weights
    selectable_idx = samplesizes_idx[samplesizes$selectable]
    selectable = samplesizes[selectable_idx,]
    min_idx = selectable_idx[selectable$diff == min(selectable$diff)]
    idx = ifelse(length(min_idx) == 1, min_idx, sample(min_idx, 1))
    samplesizes[idx,'n_'] = samplesizes[idx,'n_'] - 1
    samplesizes[idx,'selectable'] = samplesizes[idx,'n_'] > 0
    samplesizes[idx,'chosen'] = samplesizes[idx,'chosen'] + 1
    num_chosen = num_chosen + 1
    pb$tick()
  }
  samplesizes %>% select_(.dots=c(strata, 'chosen'))
}

#weights_apn =
#  residential_buildings_df %>%
#  group_by(BLK) %>%
#  summarise(n=as.numeric(n()), area=first(area)) %>%
#  mutate(weights=area/n)

#num_samples_per_block_apn = samplesizes_n_stratified(residential_buildings_df, 30000, 'BLK', weights_apn)
#sampled_per_block_apn = residential_buildings_df %>% inner_join(num_samples_per_block_apn, by='BLK') %>%
#  group_by(BLK) %>% do(sample_n(., first(.$chosen))) %>% ungroup()

weights_varpn =
  residential_buildings_df %>%
  group_by(BLK) %>%
  summarise(n=as.numeric(n()), var=sd(long)*sd(lat)) %>%
  mutate(var=ifelse(is.na(var), 1, var/mean(var, na.rm=T)), weights=var/n) #ggplot() + geom_histogram(aes(weights))

num_samples_per_block_varpn = samplesizes_n_stratified(residential_buildings_df, 30000, 'BLK', weights_varpn)
sampled_per_block_varpn = residential_buildings_df %>% inner_join(num_samples_per_block_varpn, by='BLK') %>%
  group_by(BLK) %>% do(sample_n(., first(.$chosen))) %>% ungroup()

#blk_df = tidy(blk, region='BLK')

# check how well the sampling works
#ggplot() +
#  geom_polygon(aes(long, lat, group=group), size=0.1, data=blk_df) +
#  geom_point(aes(long, lat), color='blue', size=0.01, data=residential_buildings_df) +
#  geom_point(aes(long, lat), size=0.01, color='red', data=sampled_per_block_apn) +
#  geom_point(aes(s_long, s_lat), data=re_schulstand_df) + 
#  coord_map()

#ggplot() +
#  geom_polygon(aes(long, lat, group=group), size=0.1, data=blk_df) +
#  geom_point(aes(long, lat), color='blue', size=0.01, data=residential_buildings_df) +
#  geom_point(aes(long, lat), size=0.01, color='red', data=sampled_per_block_varpn) +
#  geom_point(aes(s_long, s_lat), data=re_schulstand_df) + 
#  coord_map()
# I like this one better

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

#slice_size = 10000
#pb = progress_bar$new(total = nrow(HKO_2015_df) %/% slice_size, format = "[:bar] :current of :total / :percent / eta: :eta", clear = F)
#HKO_2015_df %>%
#  group_by(row_number() %/% slice_size) %>%
#  by_slice(~ pb$tick() %after% retry_null(osrmTable(src = .x, dst = re_schulstand_df))$durations %>% as.data.frame %>% mutate(., src=rownames(.)) %>% gather(dst, time, -src), .collate="rows", .labels=F) %>%
#  write_csv('data/routen_matrix.csv')

pb = progress_bar$new(total = nrow(sampled_per_block_varpn)*nrow(re_schulstand_df), format = "[:bar] :current of :total / :percent / eta: :eta", clear = F)
sampled_single_distances = sampled_per_block_varpn %>% mutate(x=1) %>%
  inner_join(re_schulstand_df %>% mutate(x=1), by='x') %>%
  rowwise() %>% do(pb$tick() %after% retry_null(cbind(.[c('OI', 'school_id')], do.call(data.frame, as.list(osrmRoute(c(.$OI, .$long, .$lat), c(.$school_id, .$s_long, .$s_lat), overview = F))))))

sampled_single_distances %>% write_csv('data/routen_matrix_sampled.csv')