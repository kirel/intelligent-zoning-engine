generate_tbl = function(units, entities, weights, entity_filter) {
  table_data = units %>%
    as.data.frame() %>%
    dplyr::filter(entity_id != entity_filter) %>%
    dplyr::left_join(weights, by=c('unit_id', 'entity_id')) %>%
    dplyr::group_by(entity_id) %>%
    dplyr::summarise(
      num_units = n(),
      pop = sum(population, na.rm = TRUE),
      sgbIIu65 = sum(population * sgbIIu65, na.rm = TRUE) / pop,
      min_dist = min(min, na.rm = TRUE),
      avg_dist = sum(population * avg, na.rm = TRUE) / pop,
      max_dist = max(max, na.rm = TRUE)
    ) %>%
    dplyr::left_join(entities %>%
                       as.data.frame() %>%
                       dplyr::select(entity_id, capacity, LMB, ndH),
                     by = 'entity_id') %>%
    dplyr::mutate(utilization = pop / capacity)
  return(table_data)
}

generate_spatial_df = function(units, entities, weights, entity_filter) {
  # projection
  prev_projection = sp::CRS(sp::proj4string(units))
  ea_projection = sp::CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  ea_units = sp::spTransform(units, ea_projection)
  # buffer and reverse projection
  tmp = rgeos::gBuffer(ea_units, byid = TRUE, width = 40,
                       capStyle = "FLAT", joinStyle = "MITRE") %>%
    rgeos::gUnaryUnion(id = .$entity_id) %>%
    rgeos::gBuffer(byid = TRUE, width = -40,
                   capStyle = "FLAT", joinStyle = "MITRE") %>%
    sp::spTransform(prev_projection)
  
  table_data = generate_tbl(units, entities, weights, entity_filter) %>%
    as.data.frame()
  row.names(table_data) = table_data$entity_id
  
  return(sp::SpatialPolygonsDataFrame(tmp[table_data$entity_id], table_data))
}