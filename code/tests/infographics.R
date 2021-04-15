library(tidyverse)
remotes::install_github("zonebuilders/zonebuilder") 
setwd("~/cyipt/actdev")

if(!exists("site_name")) site_name = "great-kneighton"
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

routes_summary = routes_fast %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_band = cut(x = length / 1000, distances)) %>% 
  select(distance_band, n = all_base, busyness = mean_busyness) %>% 
  group_by(distance_band) %>% 
  summarise(
    n = mean(n),
    busyness = mean(busyness)
    )

# colorspace::choose_palette()
source("code/tests/color_palette.R")
actdev_palette1_5 = function(n = 5) actdev_palette1(n = n)

brks = c(1, 1.5, 2, 3, 10)
gg_distance_busyness = routes_summary %>% 
  ggplot(aes(distance_band, n, fill = busyness)) +
  geom_bar(stat = "identity") +
  colorspace::scale_fill_binned_sequential(palette = "Red-Blue", breaks = brks, trans = "log10")

ggplot2::ggsave(file.path(path, "gg_distance_busyness.png"), gg_distance_busyness)

# geo infographic ---------------------------------------------------------

library(tidyverse)
setwd("~/cyipt/actdev")

site_name_char = site_name

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
