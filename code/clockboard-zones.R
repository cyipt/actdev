library(tidyverse)
remotes::install_github("zonebuilders/zonebuilder") 
setwd("~/cyipt/actdev")

if(!exists("site_name")) site_name = "chapelford"
sites = sf::read_sf("data-small/all-sites.geojson")
site = sites[sites$site_name == site_name, ]
path = file.path("data-small", site_name)

# input data: we should probably have naming conventions for these
list.files(path)
site_area = sf::read_sf(file.path(path, "site.geojson"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-few.geojson"))	
routes_fast = sf::read_sf(file.path(path, "routes-fast.geojson"))
routes_quiet = sf::read_sf(file.path(path, "routes-quiet.geojson"))
routes_walk = sf::read_sf(file.path(path, "routes-walk.geojson"))


# zonebuilder zones -------------------------------------------------------
distances = c(0, zonebuilder::zb_100_triangular_numbers[1:9])
summary(cut(routes_fast$length / 1000, distances))

# colorspace::choose_palette()
source("code/tests/color_palette.R")
site_centroid = site_area %>% 
  sf::st_centroid()

zones_concentric = zonebuilder::zb_zone(site_centroid, n_circles = 3)

# plot(zones_concentric)
routes_fast_broken = stplanr::line_breakup(l = routes_fast, z = zones_concentric) # slow
mapview::mapview(routes_fast_broken[1:9, ])
mapview::mapview(routes_fast_broken) +
  mapview::mapview(zones_concentric)
route_centroids = sf::st_centroid(routes) %>% 
  select(all_commute_base, mean_busyness) %>% 
  sf::st_join(zones_concentric) %>% 
  sf::st_drop_geometry() %>% 
  group_by(segment_id) %>% 
  summarise(n = sum(first(all_commute_base)), busyness = mean(routes$mean_gradient)) 

zones_concentric_joined = left_join(zones_concentric, y = route_centroids  )
zones_concentric_joined$busyness = zones_concentric_joined$busyness + runif(nrow(zones_concentric))

zones_concentric_joined %>% 
  ggplot() +
  geom_sf(aes(fill = busyness)) +
  scale_fill_gradient(low = "green", high = "red")

library(tmap)
tm_shape(zones_concentric_joined) +
  tm_polygons("busyness") +
  tm_scale_bar()

sf::st_precision(zones_concentric_joined) = 10000
zones_concentric_joined = sf::st_set_precision(zones_concentric_joined, 100000)
sf::write_sf(zones_concentric_joined, "data-small/great-kneighton/dartboard-1-3-6km.geojson")  
head(readLines("data-small/great-kneighton/dartboard.geojson"))