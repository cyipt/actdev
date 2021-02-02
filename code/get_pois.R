
get_pois = function(
  region_name,
  et = c("shop", "amenity"),
  q = "SELECT * FROM 'points' WHERE shop IN ('supermarket')",
  ...
  ) {
  # define study area
  region_name = "cambridgeshire"
  osm_points = osmextract::oe_get(region_name, query = q, extra_tags = et, force_vectortranslate = TRUE)
  # names(osm_points)
  
  # get supermarket polygons
  q_poly = gsub(pattern = "points", replacement = "multipolygons", x = q)
  osm_polygons = osmextract::oe_get(region_name, query = q_poly, extra_tags = et, force_vectortranslate = TRUE)
  
  # deduplicate supermarkets
  
  # osm_points_not_in_polygons = osm_points_sm[osm_polygons_sm, , op = sf::st_disjoint]
  osm_points_in_polygons = osm_points_sm[osm_polygons_sm, ]
  mapview::mapview(osm_points_in_polygons)
  osm_points_not_in_polygons = osm_points_sm %>% 
    filter(!osm_id %in% osm_points_in_polygons$osm_id)
  
  mapview::mapview(osm_polygons_sm) + 
    mapview::mapview(osm_points_not_in_polygons)
  
  # convert polygons to points and join together
  osm_polygons_sm_centroids = sf::st_centroid(osm_polygons_sm)
  setdiff(names(osm_points_sm), names(osm_polygons_sm_centroids))
  names_in_both = intersect(names(osm_points_sm), names(osm_polygons_sm_centroids))
  sm = rbind(osm_points_not_in_polygons[names_in_both], osm_polygons_sm_centroids[names_in_both])
  
}