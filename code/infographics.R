library(tidyverse)

if(!exists("site_name")) site_name = "great-kneighton"
path = file.path("data-small", site_name)

# input data: we should probably have naming conventions for these
list.files(path)
site_area = sf::read_sf(file.path(path, "site.geojson"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-few.geojson"))	
routes_fast = sf::read_sf(file.path(path, "routes-fast.geojson"))
routes_quiet = sf::read_sf(file.path(path, "routes-quiet.geojson"))
routes_walk = readRDS(file.path(path, "routes_walk.Rds"))

# zonebuilder zones -------------------------------------------------------
distances = c(0, zonebuilder::zb_100_triangular_numbers[1:10])
distance_bands = cut(routes_fast$length / 1000, distances)
summary(distance_bands)
distance_bands_char = as.character(distance_bands)
distance_bands_unique = unique(cut(seq(1, 50), distances))

routes_bands_df = data.frame(distance_band = distance_bands_unique)

routes_summary = routes_fast %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_band = cut(x = length / 1000, distances)) %>% 
  select(distance_band, n = all_base, busyness = mean_busyness) %>% 
  group_by(distance_band) %>% 
  summarise(
    n = mean(n),
    busyness = mean(busyness)
    ) 
routes_summary = left_join(routes_bands_df, routes_summary)
routes_summary$n[is.na(routes_summary$n)] = 0
routes_summary$busyness[is.na(routes_summary$busyness)] = 1

# colorspace::choose_palette()
# source("code/tests/color_palette.R")
actdev_palette1_5 = function(n = 5) actdev_palette1(n = n)

median(routes_fast$mean_busyness)

brks = c(1, 1.5, 2, 3, 10)
gg_distance_busyness = routes_summary %>% 
  ggplot(aes(distance_band, n, fill = busyness)) +
  geom_bar(stat = "identity") +
  scale_fill_steps2(low = "blue", mid = "purple", high = "red", midpoint = 3, limits = c(1, 5))
  
  # scale_fill_steps2(low = "blue", mid = "purple", high = "red", midpoint = 2, limits = c(1, 6))
gg_distance_busyness
  
# colorspace::scale_fill_binned_sequential(
  #   palette = "Red-Blue",
  #   breaks = brks,
  #   # breaks = waiver(),
  #   # trans = "log10",
  #   # labels = waiver()
  #   # labels = c(as.character(c(brks))) # fails
  #   # expand = TRUE,
  #   # guide = "legend",
  #   drop = FALSE
  #   )


ggplot2::ggsave(file.path(path, "gg_distance_busyness.png"), gg_distance_busyness)

# geo infographic ---------------------------------------------------------

library(tidyverse)

site_name_char = site_name

zones_db = sf::read_sf(file.path(path, "dartboard.geojson"))	

gg_db = zones_db %>% 
  ggplot(aes(fill = busyness_cycle_base)) +
  geom_sf() +
  scale_fill_steps2(low = "blue", mid = "purple", high = "red", midpoint = 3, limits = c(1, 5)) +
  theme_void()
gg_db

gg_db_walk = zones_db %>% 
  ggplot(aes(fill = circuity_walk)) +
  geom_sf() +
  scale_fill_steps2(low = "blue", mid = "purple", high = "red", midpoint = 1.3, limits = c(1, 1.6)) +
  theme_void()
gg_db_walk


ggsave(filename = file.path(path, "gg_busyness_dartboard.png"), gg_db)
ggsave(filename = file.path(path, "gg_walk_dartboard.png"), gg_db_walk)

# library(tmap)
# tm_shape(zones_db) +
#   tm_polygons("busyness_cycle_base") +
#   tm_scale_bar()
