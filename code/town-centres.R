library(sf)
library(tidyverse)
library(mapview)

# town_buffer = 5000
household_size = 2.3

town_centres = st_read("data/town-centres/English_Town_Centres_2004.shp")
st_transform(town_centres, 4326)
st_precision(town_centres) = 1000000

u = "https://github.com/cyipt/actdev/releases/download/0.1.1/all-sites.geojson"
sites = sf::st_read(u)
st_precision(sites) = 1000000

site_name = "great-kneighton"
site = sites[sites$site_name == site_name, ]
mapview(site)

# may not need to do this since centroid coordinates are already included as fields
town_centroids = town_centres %>% 
  st_transform(27700) %>% 
  st_centroid() %>% 
  st_transform(4326)

site_centroid = site %>% 
  st_transform(27700) %>% 
  st_centroid() %>% 
  st_transform(4326)

# site_buffer = stplanr::geo_buffer(site, dist = town_buffer)
# town_nearby = town_centres[site_buffer, ]

record = st_nearest_feature(site_centroid, town_centroids)
town_nearest = town_centroids[record, ]
mapview(town_nearest)

town_nearest = town_nearest %>% 
  select(town_name = NAME)

# need a better way of estimating number of trips to the town centre
od_town = data.frame(site_name = site_centroid$site_name, town_name = town_nearest$town_name, trips = site$dwellings_when_complete * household_size)

desire_line_town = od::od_to_sf(x = od_town, z = site_centroid, zd = town_nearest)
mapview(desire_line_town)

route_fast_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey)
route_balanced_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey, plan = "balanced")
route_quiet_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey, plan = "quietest")
route_walk_town = stplanr::route(l = desire_line_town, route_fun = stplanr::route_osrm)

route_fast_town = route_fast_town %>% 
  mutate(mean_gradient = weighted.mean(gradient_smooth, w = distances),
         max_gradient = max(gradient_smooth),
         busyness = busynance / distances,
         mean_busyness = weighted.mean(busyness, w = distances),
         max_busyness = max(busyness),
         across(where(is.numeric), round, 6)) %>% 
  select(site_name, town_name, trips, distances, time, speed, length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness)

route_balanced_town = route_balanced_town %>% 
  mutate(mean_gradient = weighted.mean(gradient_smooth, w = distances),
         max_gradient = max(gradient_smooth),
         busyness = busynance / distances,
         mean_busyness = weighted.mean(busyness, w = distances),
         max_busyness = max(busyness),
         across(where(is.numeric), round, 6)) %>% 
  select(site_name, town_name, trips, distances, time, speed, length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness)

route_quiet_town = route_quiet_town %>% 
  mutate(mean_gradient = weighted.mean(gradient_smooth, w = distances),
         max_gradient = max(gradient_smooth),
         busyness = busynance / distances,
         mean_busyness = weighted.mean(busyness, w = distances),
         max_busyness = max(busyness),
         across(where(is.numeric), round, 6)) %>% 
  select(site_name, town_name, trips, distances, time, speed, length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness)

dsn = file.path("data-small", site_name, "route_fast_town.geojson")
write_sf(route_fast_town, dsn = dsn)

dsn = file.path("data-small", site_name, "route_balanced_town.geojson")
write_sf(route_balanced_town, dsn = dsn)

dsn = file.path("data-small", site_name, "route_quiet_town.geojson")
write_sf(route_quiet_town, dsn = dsn)

dsn = file.path("data-small", site_name, "route_walk_town.geojson")
write_sf(route_walk_town, dsn = dsn)
