window.selector_cache = {}

cached_selector = (selector) ->
  if _.has(window.selector_cache, selector)
    window.selector_cache[selector]
  else
    elem = $(selector)
    window.selector_cache[selector] = elem if elem.length
    elem

applied = (f) -> (args) -> f(args...)

Shiny.addCustomMessageHandler 'getMapLayers', (message) ->
  console.log 'getMapLayers'
  window.mapLayers = {}
  $('#map').data("leaflet-map").eachLayer (layer) ->
    if layer instanceof L.CircleMarker
      window.mapLayers[layer.options.layerId] = layer

Shiny.addCustomMessageHandler 'updateMap', (message) ->
  console.log 'updateMap'
  console.log message
  _.zip(message.units.unit_id, message.units.entity_id, message.units.selected, message.units.locked).forEach applied(
    (unit_id, entity_id, selected, locked) ->
      unit = cached_selector('.unit-'+unit_id)
      unit
        .removeClass((index, className) -> (className.match(/(^|\s)unit-assigned-to-entity-\S+/g) or []).join ' ')
        .addClass('unit-assigned-to-entity-' + entity_id)
        .toggleClass('selected', selected)
        .toggleClass('locked', locked).toggleClass('unlocked', !locked)
        .toggleClass('other-entity-selected', message.selected_entity != '__NONE_SELECTED__' and entity_id != message.selected_entity)
    )
  scale = d3.scaleLinear().domain(d3.extent(message.entities.utilization.map((x)->Math.abs(1-x)))).range([5,8])
  _.zip(message.entities.entity_id, message.entities.utilization).forEach applied(
    (entity_id, utilization) ->
      entity = cached_selector('.entity-'+entity_id)
      entity
        .toggleClass('deselected', message.selected_entity != '__NONE_SELECTED__' and entity_id != message.selected_entity)
      cached_selector('.entity-meta.entity-'+entity_id)
        .toggleClass('over-capacity', utilization > 1)
        .toggleClass('under-capacity', utilization < 1)
      window.mapLayers['entity_meta_'+entity_id].setRadius(scale(utilization))
  )