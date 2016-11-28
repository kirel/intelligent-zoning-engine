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

HKO_2015 = readOGR('download/HKO_2015.geojson', 'OGRGeoJSON')
blk = readOGR('download/RBS_OD_BLK_2015_12.geojson', 'OGRGeoJSON')
alkis = readOGR('download/re_alkis_tatsaechlichenutzungflaechen.geojson', layer = 'OGRGeoJSON')

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

sampled_per_block_varpn %>% select(OI, long, lat, BLK) %>% write_rds('output/sampled_buildings.rds', 'gz')
