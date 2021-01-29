library(tidyverse)
setwd("~/cyipt/actdev")

sites = sf::read_sf("data-small/all-sites.geojson")
routes = sf::read_sf(file.path("data-small", site_name, "routes-fast.geojson"))
distances = c(0, zonebuilder::zb_100_triangular_numbers[1:9])
summary(cut(routes$distance_m * 1000, distances))

routes_summary = routes %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_band = cut(x = distance_m / 1000, distances)) %>% 
  group_by(geo_code2, distance_band) %>% 
  # todo: update next line to show busyness as busynance/distance
  summarise(n = sum(first(all_commute_base)), busyness = mean(mean_busyness)) %>% 
  select(distance_band, n, busyness) %>% 
  group_by(distance_band) %>% 
  summarise(across(n:busyness, sum))

routes_summary

routes_summary %>% 
  ggplot(aes(distance_band, n, fill = busyness)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "green", high = "red")


# geo infographic ---------------------------------------------------------

remotes::install_github("zonebuilders/zonebuilder") # not needed, for reference

library(tidyverse)
setwd("~/cyipt/actdev")

site_name_char = "great-kneighton"
sites = sf::read_sf("data-small/all-sites.geojson")
site_centroid = sites %>% 
  filter(site_name == site_name_char) %>% 
  sf::st_centroid()

zones_concentric = zonebuilder::zb_zone(site_centroid, n_circles = 3)

plot(zones_concentric)
routes = sf::read_sf(file.path("data-small", site_name_char, "routes-fast.geojson"))
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
