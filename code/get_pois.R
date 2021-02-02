
get_pois = function(
  region_name,
  et = c("shop", "amenity"),
  q = "SELECT * FROM 'points' WHERE shop IN ('supermarket')",
  ...
  ) {
  # define study area (tests)
  # region_name = "cambridgeshire"
  # et = c("shop", "amenity")
  # q = "SELECT * FROM 'points' WHERE shop IN ('supermarket')"
  
  osm_points = osmextract::oe_get(region_name, query = q, extra_tags = et, force_vectortranslate = TRUE)
  # names(osm_points)
  
  # get supermarket polygons
  q_poly = gsub(pattern = "points", replacement = "multipolygons", x = q)
  osm_polygons = osmextract::oe_get(region_name, query = q_poly, extra_tags = et, force_vectortranslate = TRUE)
  
  # deduplicate supermarkets
  require(sf)
  if(nrow(osm_polygons) > 0) {
    osm_points_in_polygons = osm_points[osm_polygons, ]
    mapview::mapview(osm_points_in_polygons)
    osm_points_not_in_polygons = osm_points[
      !osm_points$osm_id %in% osm_points_in_polygons$osm_id,
    ] 
    
    # convert polygons to points and join together
    osm_polygons_centroids = sf::st_centroid(osm_polygons)
    setdiff(names(osm_points), names(osm_polygons_centroids))
    names_in_both = intersect(names(osm_points), names(osm_polygons_centroids))
    osm_points = rbind(osm_points_not_in_polygons[names_in_both], osm_polygons_centroids[names_in_both])
  } 
  osm_points
}

# tests
pois_hereford = get_pois(region_name = "hereford")
mapview::mapview(pois_hereford)
mapview::mapview(get_pois(region_name = "west yorkshire"))
