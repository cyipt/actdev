# Aim get pois from osm in a way scales

remotes::install_github("itsleeds/osmextract")

library(osmextract)
library(dplyr)

# define study area
region_name = "cambridgeshire"
osm_points = oe_get(place = region_name, layer = "points")
# todo: get points with sql
osm_points %>% 
  sample_n(size = 1000) %>% 
  mapview::mapview()

# supermarkets
names(osm_points)
# oh no! we don't have shops - re-create 
et = c("shop", "amenity")
osm_points = oe_get(place = region_name, layer = "points", extra_tags = et)
names(osm_points)

top_points = osm_points %>% 
  sf::st_drop_geometry() %>% 
  group_by(shop) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

top_points

osm_points_sm = osm_points %>% 
  filter(shop == "supermarket")

mapview::mapview(osm_points_sm)

# get supermarket polygons
osm_polygons = oe_get(place = "west yorkshire", layer = "multipolygons")
osm_polygons = oe_get(place = region_name, layer = "multipolygons", force_vectortranslate = TRUE)
table(osm_polygons$shop)
osm_polygons_sm = osm_polygons %>% 
  filter(shop == "supermarket")

mapview::mapview(osm_polygons_sm) + 
  mapview::mapview(osm_points_sm)

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
mapview::mapview(sm)

# next stage: create function
# osm_to_points = function(osm_points, osm_other) {
unique_centroids = function(osm_points, osm_other) {
  # add code here
  # return("banana")
  "banana"
}
