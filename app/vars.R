plan(multiprocess)

MIN_UTILIZATION = 0.9
MAX_UTILIZATION = 1.1

# Load data
units = readOGR('data/units.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
entities = readOGR('data/entities.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE)
weights = read_csv('data/weights.csv')
adjacency = read_csv('data/adjacency.csv', col_types ='cc')

addresses = read_rds('data/addresses.rds')
addresses_add = read_rds('data/addresses_add.rds')

# Add coordinates to adjacency data frame - just for debugging / visualization
row.names(units) = units$unit_id
adjacency = adjacency %>%
  inner_join(units %>% coordinates() %>% as.data.frame() %>% rename(from_long=V1, from_lat=V2) %>% mutate(from=rownames(.))) %>%
  inner_join(units %>% coordinates() %>% as.data.frame() %>% rename(to_long=V1, to_lat=V2) %>% mutate(to=rownames(.)))

assignment = units@data %>%
  select(unit_id) %>%
  left_join(read_csv('data/assignment.csv'), by='unit_id') %>%
  mutate(entity_id = ifelse(is.na(entity_id), NO_ASSIGNMENT, entity_id))

units = units %>% sp::merge(assignment)

entities_df = entities %>% as.data.frame()

rm(assignment)

bez = readOGR('data/RBS_OD_BEZ_2015_12.geojson', layer = 'OGRGeoJSON', stringsAsFactors = FALSE) %>% subset(BEZ == '07')

entity_ids = unique(entities$entity_id)
unit_ids = unique(units$unit_id)
optimizable_units = units %>% as.data.frame %>% filter(population > 0) %>% inner_join(weights, by='unit_id') %>% .$unit_id %>% unique
population_range = range(units$population, na.rm = T, finite = T)

flog.debug('Data loading complete')

### Helper functions

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# preferrably sample blocks closer to the school
# should be higher for smaller numbers
distance_sample_weights = unit_ids %>% map(function(unit_id) {
  idx = weights$unit_id == unit_id
  avg = as.double(weights[idx,]$avg)
  entity_ids = weights[idx,]$entity_id
  # TODO make function and divide exponent by optimization step
  weights = (1-(avg-min(avg))/max(avg-min(avg)))^3
  set_names(weights, entity_ids)
}) %>% set_names(unit_ids)

distance_sample_probs = function(unit_id, heuristic_exponent=3) {
  weights = distance_sample_weights[[unit_id]]
  pot_weights = weights^heuristic_exponent
  pot_weights/sum(pot_weights)
}

unit_index = set_names(1:length(units$unit_id), units$unit_id)

entities$selected = F
# Remark: There is no entities$updated because they have to be redrawn anyway
entities$highlighted = T # highlight when selected
entities$hovered = F
units$selected = F # selected units can be reassigned, locked, etc
units$highlighted = T # highlight units when assigned entity is selected
units$locked = F
units$updated = T # only updated unites need to be redrawn (true for initial render)
units$hovered = F

next_unique_name = function(from_name, forbidden, postfix = 'copy') {
  c('', seq(1000)+1) %>% purrr::map(~ trimws(paste(from_name, postfix, .x))) %>% purrr::detect(~ !.x %in% forbidden)
}