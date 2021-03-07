# Aim: generate scenarios of change associated with new developments

library(tidyverse)
library(sf)
library(stplanr)
remotes::install_github("itsleeds/od")
library(od)

# set-up and parameters ---------------------------------------------------

# setwd("~/cyipt/actdev") # run this script from the actdev folder
if(!exists("site_name")) { # assume all presets loaded if site_name exists
  site_name = "lcid"   # which site to look at (can change)
  data_dir = "data-small" # for test sites
  max_length = 20000 # maximum length of desire lines in m
  household_size = 2.3 # mean UK household size at 2011 census
  min_flow_routes = 10 # threshold above which OD pairs are included
  region_buffer_dist = 2000
  large_area_buffer = 500
}

if(!exists("centroids_msoa")) {
  # run the build script if national data is missing
  source("code/build-setup.R")
}

# Select site of interest -------------------------------------------------
site = sites[sites$site_name == site_name, ]
message("Building for ", site$site_name)

path = file.path(data_dir, site_name)
# dir.create(path = path)

zones_touching_site = zones_msoa_national[site, , op = sf::st_intersects]

zones_touching_site$overlap_size = units::drop_units(st_area(zones_touching_site))
zones_touching_site = zones_touching_site %>% 
  filter(overlap_size > 10000) %>% 
  select(-overlap_size)

# Route from site centroid (rather than MSOA centroid) --------------------
# `disaggregate.R` changes this to route from a random selection of homes within the site, to better represent the accessibility of the site as a whole
site_centroid = site %>% 
  st_transform(27700) %>% 
  st_centroid() %>% 
  st_transform(4326)

zone_data = zones_touching_site %>% 
  st_drop_geometry() %>%
  mutate(site_name = site$site_name)
site_c = right_join(site_centroid, zone_data) %>%
  select(geo_code, site_name)

# Generate desire lines ---------------------------------------------------
od_site = od %>% 
  filter(geo_code1 %in% zones_touching_site$geo_code) %>% 
  filter(geo_code2 %in% centroids_msoa$msoa11cd)
# intra-zonal flows are included, with the desire line going from the site centroid to the msoa centroid
# intra-zonal flows could later be represented using more detailed od-workplace zone data.

desire_lines_site = od::od_to_sf(x = od_site, z = site_c, zd = centroids_msoa)
desire_lines_site = desire_lines_site %>% 
  mutate(site_name = site_name)

# Adjust flows to represent site population, not MSOA population(s) -------
# for both MSOAs and development sites, these are entire populations, not commuter populations
desire_lines_site = inner_join(desire_lines_site, msoa_pops)
desire_lines_site = inner_join(desire_lines_site, site_pops)
site_population = unique(desire_lines_site$site_population)
unique_msoa_pops = desire_lines_site %>% 
  st_drop_geometry() %>% 
  select(geo_code1, msoa_population) %>%
  unique()
sum_msoa_pops = sum(unique_msoa_pops$msoa_population)
desire_lines_site = desire_lines_site %>% 
  mutate(sum_msoa_pops = sum_msoa_pops)

# keeping converted flows in the original columns
desire_lines_pops = desire_lines_site %>% 
  mutate(across(all:other, .fns = ~ ./ sum_msoa_pops * site_population))

# todo: add empirical data on 'new homes' effect (residents of new homes are more likely to drive than residents of older homes)
# could also adjust the base walking and cycling mode shares in response to the difference between journey distance from the site centroid as compared to journey distance from the MSOA centroid (eg in Cambridge, the MSOA centroid is a fair bit closer to the city centre than the site centroid, which could explain why such a high proportion of commuters are shown walking to work in the city centre)  

# For sites with 2 or more origin MSOAs, combine flows to avoid having multiple desire lines to the same destination MSOA
desire_lines_combined = desire_lines_pops %>% 
  group_by(geo_code2) %>% 
  summarise(
    geo_code1 = geo_code1[1], # do we even need this?
    across(all:other, sum)
  )

desire_lines_combined$length = round(stplanr::geo_length(desire_lines_combined))

# todo: add PT
desire_lines_combined = desire_lines_combined %>% 
  mutate(
    trimode_base = foot + bicycle + car_driver,
    pwalk_base = foot/trimode_base,
    pcycle_base = bicycle/trimode_base,
    pdrive_base = car_driver/trimode_base
    )
desire_lines_combined[is.na(desire_lines_combined)] = 0

desire_lines_combined = desire_lines_combined %>% 
  select(geo_code1, geo_code2, all, trimode_base, from_home:other, length, pwalk_base:pdrive_base) %>%
  mutate(across(where(is.numeric), round, 6))

st_precision(desire_lines_combined) = 1000000

dsn = file.path(data_dir, site_name, "all-census-od.csv")
obj = st_drop_geometry(desire_lines_combined)
if(file.exists(dsn)) file.remove(dsn)
if(!dir.exists(path)) dir.create(path)
readr::write_csv(obj, file = dsn)

# Round decimals and select sets of desire lines --------------------------
desire_lines_rounded = desire_lines_combined %>% 
  rename(all_base = all, walk_base = foot, cycle_base = bicycle, drive_base = car_driver) %>%
  mutate(
    across(all_base:other, smart.round),
    trimode_base = walk_base + cycle_base + drive_base
    ) %>% 
  filter(trimode_base > 0)

desire_lines_20km = desire_lines_rounded %>% 
  filter(length <= max_length)

desire_lines_threshold = desire_lines_rounded %>%
  filter(trimode_base >= min_flow_routes)

desire_lines_bounding = desire_lines_20km %>% 
  filter(geo_code2 %in% desire_lines_threshold$geo_code2)

# Large study area MSOAs --------------------------------------------------
large_study_area = sf::st_convex_hull(sf::st_union(desire_lines_bounding))
large_study_area = stplanr::geo_buffer(large_study_area, dist = large_area_buffer)

dsn = file.path(data_dir, site_name, "large-study-area.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(large_study_area, dsn = dsn)

desire_lines_many = desire_lines_rounded[large_study_area, , op = sf::st_within]

desire_lines_many = desire_lines_many %>% 
  select(geo_code1, geo_code2, all_base, trimode_base, walk_base, cycle_base, drive_base, length, pwalk_base:pdrive_base)

# for future reference when creating the abstr scenarios
saveRDS(desire_lines_many, file.path(path, "desire_lines_many.Rds"))

if(!exists("disaggregate_desire_lines"))
  disaggregate_desire_lines = FALSE

if(disaggregate_desire_lines && nrow(desire_lines_many) < 20) {
  zones_many = zones_msoa_national %>% filter(geo_code %in% desire_lines_many$geo_code2)
  centroids_lsoa_national = pct::get_pct(layer = "c", national = TRUE)
  zones_lsoa_national = pct::get_pct(layer = "z", national = TRUE)
  centroids_lsoa_many = centroids_lsoa_national[zones_many, ]
  zones_lsoa_many = centroids_lsoa_national %>% filter(geo_code %in% centroids_lsoa_many$geo_code)
  desire_lines_many_min = desire_lines_many %>% select(geo_code1:drive_base) %>% sf::st_drop_geometry()
  n_lines = max(nrow(desire_lines_many), 20) # minimum of 20 desire lines
  p = sum(desire_lines_many_min$all_base) / n_lines
  z = rbind(
    zones_many %>% select(geo_code),
    site %>% transmute(geo_code = site$site_name)
  )
  g = c(
    sf::st_sample(zones_many, rep(20, nrow(zones_many))),
    sf::st_sample(site, 20)
    )
  d = data.frame(id = paste0("i", 1:length(g)))
  sp = sf::st_sf(d, g)
  # mapview::mapview(sp) + mapview::mapview(z)
  desire_lines_many_min$geo_code1 = site$site_name
  # desire_lines_many_commute = od::od_disaggregate(od = desire_lines_many_min, z = zones_many, subzones = zones_lsoa_many) 
  # file.edit("~/itsleeds/od/R/aggregate.R") # edit locally for testing + debugging
  # summary(desire_lines_many_min$geo_code1 %in% z$geo_code)
  # summary(desire_lines_many_min$geo_code2 %in% z$geo_code)
  # summary(z$geo_code %in% desire_lines_many_min$geo_code2)
  trip_attractors = sf::read_sf(file.path(path, "trip_attractors.geojson"))
  houses = sf::read_sf(file.path(path, "site_buildings.geojson"))
  sz = rbind(trip_attractors, houses)
  sz[[1]] = paste0("i", 1:nrow(sz))
  zones_with_buildings = z[sz, ]
  zones_without_buildings = z %>% filter(!geo_code %in% zones_with_buildings$geo_code)
  n_without = nrow(zones_without_buildings)
  if(n_without > 0) {
    message("Randomly selecting destinations")
    n_buildings_per_zone = rep(10, n_without)
    p_sample = sf::st_sample(zones_without_buildings, n_buildings_per_zone)
    d = sz %>% sf::st_drop_geometry() %>% sample_n(length(p_sample), replace = TRUE)
    d$osm_way_id = paste0("synthetic", 1:length(p))
    b = sf::st_sf(d, geometry = stplanr::geo_buffer(p_sample, dist = 50))
    sz = rbind(sz, b)
  }
  
  # mapview::mapview(sz) + mapview::mapview(zones_with_buildings) + mapview::mapview(zones_without_buildings)
  # Route to random points:
  # desire_lines_disag = od_disaggregate(od = desire_lines_many_min, z = z, subpoints = sp, population_per_od = p)
  # Route to buildings:
  desire_lines_disag = od_disaggregate(od = desire_lines_many_min, z = z, subzones = sz, population_per_od = p)
  
  desire_lines_many = desire_lines_disag %>% 
    mutate(
      pwalk_base = walk_base/trimode_base,
      pcycle_base = cycle_base/trimode_base,
      pdrive_base = drive_base/trimode_base
    ) %>% 
    rename(geo_code1 = o, geo_code2 = d)
  desire_lines_many$length = stplanr::geo_length(desire_lines_many)
  
  # # sanity tests  
  # sum(desire_lines_many$all_base) == sum(desire_lines_disag$all_base)
  # sum(desire_lines_many$walk_base) == sum(desire_lines_disag$walk_base)
  # library(tmap)
  # tmap_mode("view")
  # qtm(desire_lines_disag) + qtm(site)
  
}

# Create routes and generate Go Dutch scenario ---------------------
obj = desire_lines_many %>% select(-length)
obj2 = desire_lines_many %>% filter(length < 6000) %>% select(-length)

routes_fast = stplanr::route(l = obj, route_fun = cyclestreets::journey)
routes_balanced = stplanr::route(l = obj, route_fun = cyclestreets::journey, plan = "balanced")
routes_quiet = stplanr::route(l = obj, route_fun = cyclestreets::journey, plan = "quietest")
routes_walk = stplanr::route(l = obj2, route_fun = stplanr::route_osrm)

# # name = paste0(site_name, "-routes-walk.geojson")
# # sf::write_sf(routes_walk, name) #save it just in case, to avoid repeatedly calling API
# routes_walk = stplanr::route(l = obj2, route_fun = stplanr::route_google, mode = "walking") 
# # save as Rds files for future references, e.g. for dartboard, but not for app:
saveRDS(routes_walk, file.path(path, "routes_walk.Rds"))
saveRDS(routes_fast, file.path(path, "routes_fast.Rds"))
saveRDS(routes_balanced, file.path(path, "routes_balanced.Rds"))
saveRDS(routes_quiet, file.path(path, "routes_quiet.Rds"))

# to reload the data and avoid re-routing
# routes_fast = readRDS(file.path(path, "routes_fast.Rds"))
# routes_balanced = readRDS(file.path(path, "routes_balanced.Rds"))
# routes_quiet = readRDS(file.path(path, "routes_quiet.Rds"))
# routes_walk = readRDS(file.path(path, "routes_walk.Rds"))

# create routes_fast
routes_fast = routes_fast %>%
  mutate(busyness = busynance / distances) %>% 
  group_by(geo_code1, geo_code2) %>%
  mutate(
    n = n(), #could remove
    mean_gradient = weighted.mean(gradient_smooth, distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, distances), 
    max_busyness = max(busyness)
  ) %>%
  ungroup() %>% 
  mutate(
    pcycle_godutch_uptake = pct::uptake_pct_godutch_2020(distance = length, gradient = mean_gradient),
    cycle_godutch_additional = pcycle_godutch_uptake * drive_base,
    cycle_godutch = cycle_base + cycle_godutch_additional,
    pcycle_godutch = cycle_godutch / all_base,
    drive_godutch = drive_base - cycle_godutch_additional, # ensure totals add up
    across(c(mean_gradient, max_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_godutch), round, 6)
  )

# to round cycle_godutch
routes_fast_summarised = routes_fast %>% 
  st_drop_geometry() %>% 
  group_by(geo_code2) %>% 
  summarise(
    all_base = mean(all_base),
    trimode_base = mean(trimode_base),
    cycle_base = mean(cycle_base),
    cycle_godutch = mean(cycle_godutch)
    )
routes_fast_summarised = routes_fast_summarised %>% 
  mutate(cycle_godutch = smart.round(cycle_godutch))

# routes_fast_summarised = routes_fast_summarised %>% 
#   filter(cycle_base > 0 | cycle_godutch > 0) # remove routes with no cyclists

routes_fast = inner_join((routes_fast %>% select(-all_base, -trimode_base, -cycle_base, -cycle_godutch)), routes_fast_summarised)

# balanced routes
routes_balanced = routes_balanced %>%
  mutate(busyness = busynance / distances) %>% 
  group_by(geo_code1, geo_code2) %>%
  mutate(
    n = n(), #could remove
    mean_gradient = weighted.mean(gradient_smooth, distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, distances),
    max_busyness = max(busyness)
  ) %>%
  ungroup() %>% 
  mutate(
    pcycle_godutch_uptake = pct::uptake_pct_godutch_2020(distance = length, gradient = mean_gradient),
    cycle_godutch = pcycle_godutch_uptake * drive_base + cycle_base,
    pcycle_godutch = cycle_godutch / all_base,
    across(c(mean_gradient, max_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_godutch), round, 6)
  )

# to round cycle_godutch
routes_balanced_summarised = routes_balanced %>% 
  st_drop_geometry() %>% 
  group_by(geo_code2) %>% 
  summarise(
    all_base = mean(all_base),
    trimode_base = mean(trimode_base),
    cycle_base = mean(cycle_base),
    cycle_godutch = mean(cycle_godutch)
    )
routes_balanced_summarised = routes_balanced_summarised %>% 
  mutate(cycle_godutch = smart.round(cycle_godutch))

routes_balanced_summarised = routes_balanced_summarised %>% 
  filter(cycle_base > 0 | cycle_godutch > 0) # remove routes with no cyclists under Go Dutch

routes_balanced = inner_join((routes_balanced %>% select(-all_base, -trimode_base, -cycle_base, -cycle_godutch)), routes_balanced_summarised)

# quiet routes
routes_quiet = routes_quiet %>%
  mutate(busyness = busynance / distances) %>% 
  group_by(geo_code1, geo_code2) %>%
  mutate(
    n = n(), #could remove
    mean_gradient = weighted.mean(gradient_smooth, distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, distances),
    max_busyness = max(busyness)
  ) %>%
  ungroup() %>% 
  mutate(
    pcycle_godutch = pct::uptake_pct_godutch_2020(distance = length, gradient = mean_gradient),
    cycle_godutch = pcycle_godutch * trimode_base,
    across(c(mean_gradient, max_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_godutch), round, 6)
  )

# to round cycle_godutch
routes_quiet_summarised = routes_quiet %>% 
  st_drop_geometry() %>% 
  group_by(geo_code2) %>% 
  summarise(
    all_base = mean(all_base),
    trimode_base = mean(trimode_base),
    cycle_base = mean(cycle_base),
    cycle_godutch = mean(cycle_godutch)
    )
routes_quiet_summarised = routes_quiet_summarised %>% 
  mutate(cycle_godutch = smart.round(cycle_godutch))

routes_quiet_summarised = routes_quiet_summarised %>% 
  filter(cycle_base > 0 | cycle_godutch > 0) # remove routes with no cyclists

routes_quiet = inner_join((routes_quiet %>% select(-all_base, -trimode_base, -cycle_base, -cycle_godutch)), routes_quiet_summarised)

# Walking routes
if(is.null(routes_walk$distance)) {
  # change names if routing service used different names
  # but for google these columns are full of NAs
  routes_walk = routes_walk_save %>% 
    mutate(distance = distance_m, duration = duration_s)
}
routes_walk_save = routes_walk %>% 
  filter(distance <= 6000) %>%
  mutate(
    # pwalk_base = walk_base / trimode_base,
    pwalk_godutch = case_when(	
      distance <= 2000 ~ pwalk_base + 0.3, # 30% shift walking for routes >2km
      distance <= 2500 ~ pwalk_base + 0.2, # 20% shift walking for routes >2.5km
      distance <= 3000 ~ pwalk_base + 0.1, # 10% shift walking for routes >3km
      distance <= 6000 ~ pwalk_base + 0.05, # 5% shift walking for routes 3-6km	
      TRUE ~ pwalk_base),
    walk_godutch = pwalk_godutch * trimode_base
  ) 

routes_walk_save = routes_walk_save %>%
  mutate(walk_godutch = smart.round(walk_godutch)) %>%
  mutate(pwalk_godutch = round(pwalk_godutch, 6))

all_commuters_baseline = sum(routes_fast_summarised$all_base)
trimode_commuters_baseline = sum(routes_fast_summarised$trimode_base)
cycle_commuters_baseline = sum(routes_fast_summarised$cycle_base)
cycle_commuters_godutch = sum(routes_fast_summarised$cycle_godutch)
walk_commuters_baseline = sum(routes_walk_save$walk_base)
walk_commuters_godutch = sum(routes_walk_save$walk_godutch)
# drive_commuters_baseline = sum(routes_fast_summarised$drive_base)
# drive_commuters_godutch = sum(routes_fast_summarised$drive_godutch)

# Route to town centre ----------------------------------------------------
bng_site = site_centroid %>% st_transform(27700)
bng_town = town_centroids %>% st_transform(27700)
# record = case_when(
#   site_name == "ebbsfleet" ~ as.integer(860),
#   TRUE ~ st_nearest_feature(bng_site, bng_town)
# )
record = st_nearest_feature(bng_site, bng_town)

town_nearest = town_centroids[record, ]
# mapview(town_nearest)
town_nearest = town_nearest %>% 
  select(town_name = NAME)

# number of trips to the town centre estimated to equal the total number of commuter trips
# todo: change the godutch so this is calculated instead of assumed
od_town = data.frame(
  site_name = site_centroid$site_name, 
  town_name = town_nearest$town_name, 
  all_base = all_commuters_baseline, 
  trimode_base = trimode_commuters_baseline,
  walk_base = walk_commuters_baseline, 
  walk_godutch = walk_commuters_godutch,
  cycle_base = cycle_commuters_baseline, 
  cycle_godutch = cycle_commuters_godutch
)

desire_line_town = od::od_to_sf(x = od_town, z = site_centroid, zd = town_nearest)
# mapview(desire_line_town)

route_fast_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey)
route_balanced_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey, plan = "balanced")
route_quiet_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey, plan = "quietest")
# working again for a single route. We want this for the desire line, even if it's over 6km
if(walk_commuters_baseline > 0 | walk_commuters_godutch > 0) {
  route_walk_town = stplanr::route(l = desire_line_town, route_fun = stplanr::route_osrm)
}

fast_town = route_fast_town %>% 
  mutate(
    busyness = busynance / distances,
    mean_gradient = weighted.mean(gradient_smooth, w = distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, w = distances),
    max_busyness = max(busyness),
    pcycle_godutch = cycle_godutch / trimode_base,
    across(where(is.numeric), round, 6)
    ) %>% 
  select(site_name, town_name, all_base, trimode_base, cycle_base, cycle_godutch, distances, 
         # time, speed, 
         length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness, pcycle_godutch)

balanced_town = route_balanced_town %>% 
  mutate(
    busyness = busynance / distances,
    mean_gradient = weighted.mean(gradient_smooth, w = distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, w = distances),
    max_busyness = max(busyness),
    pcycle_godutch = cycle_godutch / trimode_base,
    across(where(is.numeric), round, 6)
  ) %>% 
  select(site_name, town_name, all_base, trimode_base, cycle_base, cycle_godutch, distances, 
         # time, speed, 
         length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness, pcycle_godutch)

quiet_town = route_quiet_town %>% 
  mutate(
    busyness = busynance / distances,
    mean_gradient = weighted.mean(gradient_smooth, w = distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, w = distances),
    max_busyness = max(busyness),
    pcycle_godutch = cycle_godutch / trimode_base,
    across(where(is.numeric), round, 6)
  ) %>% 
  select(site_name, town_name, all_base, trimode_base, cycle_base, cycle_godutch, distances, 
         # time, speed, 
         length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness, pcycle_godutch)

# Combine and save routes -------------------------------------------------
routes_fast_cutdown = routes_fast %>%
  select(geo_code1, geo_code2, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

fast_town_cutdown = fast_town %>% 
  select(geo_code1 = site_name, geo_code2 = town_name, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

routes_fast_combined = bind_rows(
  routes_fast_cutdown %>% mutate(purpose = "commute"),
  fast_town_cutdown %>% mutate(purpose = "town")
  )

routes_fast_entire = routes_fast_combined %>% 
  group_by(geo_code1, geo_code2, purpose, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, pcycle_godutch) %>% 
  summarise() %>% 
  arrange(cycle_base)

# routes_fast_des = routes_fast_entire %>% 
#   select(geo_code2, purpose, cycle_godutch, pcycle_godutch)

dsn = file.path(data_dir, site_name, "routes-fast.geojson")
obj = routes_fast_entire %>%  select(-pcycle_godutch)
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(obj = obj, dsn = dsn)

# balanced routes
routes_balanced_cutdown = routes_balanced %>%
  select(geo_code1, geo_code2, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

balanced_town_cutdown = balanced_town %>% 
  select(geo_code1 = site_name, geo_code2 = town_name, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

routes_balanced_combined = bind_rows(
  routes_balanced_cutdown %>% mutate(purpose = "commute"),
  balanced_town_cutdown %>% mutate(purpose = "town")
)

routes_balanced_entire = routes_balanced_combined %>% 
  group_by(geo_code1, geo_code2, purpose, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, pcycle_godutch) %>% 
  summarise() %>% 
  arrange(cycle_base)

# routes_balanced_des = routes_balanced_entire %>% 
#   select(geo_code2, cycle_godutch, pcycle_godutch)

dsn = file.path(data_dir, site_name, "routes-balanced.geojson")
obj = routes_balanced_entire %>%  select(-pcycle_godutch)
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(obj = obj, dsn = dsn)

# quiet routes
routes_quiet_cutdown = routes_quiet %>%
  select(geo_code1, geo_code2, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

quiet_town_cutdown = quiet_town %>% 
  select(geo_code1 = site_name, geo_code2 = town_name, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

routes_quiet_combined = bind_rows(
  routes_quiet_cutdown %>% mutate(purpose = "commute"),
  quiet_town_cutdown %>% mutate(purpose = "town")
)

routes_quiet_entire = routes_quiet_combined %>% 
  group_by(geo_code1, geo_code2, purpose, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, pcycle_godutch) %>% 
  summarise() %>% 
  arrange(cycle_base)

# routes_quiet_des = routes_quiet_entire %>% 
#   select(geo_code2, cycle_godutch, pcycle_godutch)

dsn = file.path(data_dir, site_name, "routes-quiet.geojson")
obj = routes_quiet_entire %>%  select(-pcycle_godutch)
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(obj = obj, dsn = dsn)

# walking routes
if(walk_commuters_baseline > 0 | walk_commuters_godutch > 0) {
  # combine commute and town routes
  routes_walk_cutdown = routes_walk_save %>%
    select(geo_code1, geo_code2, distance, duration, all_base, trimode_base, walk_base, walk_godutch, pwalk_godutch)
  
  walk_town_cutdown = route_walk_town %>% 
    mutate(pwalk_godutch = round(walk_godutch / trimode_base, 6)) %>% 
    select(geo_code1 = site_name, geo_code2 = town_name, distance, duration, all_base, trimode_base, walk_base, walk_godutch, pwalk_godutch)
  
  routes_walk_combined = routes_walk_cutdown %>% mutate(purpose = "commute")
  
  if(route_walk_town$distance <= 6000) routes_walk_combined = bind_rows(routes_walk_combined, walk_town_cutdown %>% mutate(purpose = "town"))
  
  # create object for rnet and to save (desire lines simply use routes_walk_save)
  walk_obj = routes_walk_combined %>%
    select(geo_code1, geo_code2, purpose, distance, duration, all_base, trimode_base, walk_base, walk_godutch)
  
  dsn = file.path(data_dir, site_name, "routes-walk.geojson")
  if(file.exists(dsn)) file.remove(dsn)
  sf::write_sf(walk_obj, dsn = dsn)
}

# Route networks ----------------------------------------------------------
rnet_fast = overline(routes_fast_combined, attrib = c("cycle_base", "cycle_godutch", "busyness", "gradient_smooth"), fun = c(sum, mean))
rnet_fast = rnet_fast %>% 
  select(cycle_base = cycle_base_fn1, cycle_godutch = cycle_godutch_fn1, busyness = busyness_fn2, gradient_smooth = gradient_smooth_fn2) %>% 
  mutate(gradient_smooth = round(gradient_smooth, 6)) %>% 
  rename(gradient = gradient_smooth)
# nrow(rnet_fast)
# mapview::mapview(rnet_fast["cycle_base"])

dsn = file.path(data_dir, site_name, "rnet-fast.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(rnet_fast, dsn = dsn)

rnet_balanced = overline(routes_balanced_combined, attrib = c("cycle_base", "cycle_godutch", "busyness", "gradient_smooth"), fun = c(sum, mean))
rnet_balanced = rnet_balanced %>% 
  select(cycle_base = cycle_base_fn1, cycle_godutch = cycle_godutch_fn1, busyness = busyness_fn2, gradient_smooth = gradient_smooth_fn2) %>% 
  mutate(gradient_smooth = round(gradient_smooth, 6)) %>% 
  rename(gradient = gradient_smooth)

dsn = file.path(data_dir, site_name, "rnet-balanced.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(rnet_balanced, dsn = dsn)

rnet_quiet = overline(routes_quiet_combined, attrib = c("cycle_base", "cycle_godutch", "busyness", "gradient_smooth"), fun = c(sum, mean))
rnet_quiet = rnet_quiet %>% 
  select(cycle_base = cycle_base_fn1, cycle_godutch = cycle_godutch_fn1, busyness = busyness_fn2, gradient_smooth = gradient_smooth_fn2) %>% 
  mutate(gradient_smooth = round(gradient_smooth, 6)) %>% 
  rename(gradient = gradient_smooth)

dsn = file.path(data_dir, site_name, "rnet-quiet.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(rnet_quiet, dsn = dsn)

if(walk_commuters_baseline > 0 | walk_commuters_godutch > 0) {
  # r_walk_grouped_lines = routes_walk_combined %>% st_cast("LINESTRING") #is this needed?
  rnet_walk = overline(routes_walk_combined, attrib = c("walk_base", "walk_godutch", "duration"), fun = c(sum, mean))
  rnet_walk = rnet_walk %>% 
    select(walk_base = walk_base_fn1, walk_godutch = walk_godutch_fn1, duration = duration_fn2)
  dsn = file.path(data_dir, site_name, "rnet-walk.geojson")
  if(file.exists(dsn)) file.remove(dsn)
  sf::write_sf(rnet_walk, dsn = dsn)
}

# Go Dutch scenario for desire lines -------------------------------------

# get the go dutch flows for cycle and walk commutes
join_fast = routes_fast_entire %>% 
  select(geo_code2, purpose, cycle_godutch, pcycle_godutch) %>% 
  st_drop_geometry()
desire_lines_scenario = inner_join(desire_lines_many, join_fast, by = "geo_code2")

join_walk = routes_walk_combined %>% 
  st_drop_geometry() %>% 
  select(geo_code2, walk_godutch, pwalk_godutch)
desire_lines_scenario = left_join(desire_lines_scenario, join_walk, by = "geo_code2")

desire_lines_scenario = desire_lines_scenario %>% 
  select(geo_code1, geo_code2, purpose, all_base:length, walk_godutch, pwalk_godutch, cycle_godutch, pcycle_godutch)

# same number to town as commute - simplifying assumption
drive_commuters_baseline = sum(desire_lines_scenario$drive_base)

# add in a desire line to the town centre
desire_line_town = desire_line_town %>%
  mutate(
    purpose = "town",
    drive_base = drive_commuters_baseline,
    length = round(stplanr::geo_length(desire_line_town)),
    pwalk_godutch = walk_godutch / trimode_base,
    pcycle_godutch = cycle_godutch / trimode_base
    
    ) %>% 
  rename(geo_code1 = site_name, geo_code2 = town_name) %>%
  select(geo_code1, geo_code2, purpose, all_base, trimode_base, walk_base, cycle_base, drive_base, length, walk_godutch, pwalk_godutch, cycle_godutch, pcycle_godutch)

desire_lines_final = bind_rows(
  desire_lines_scenario,
  desire_line_town
) %>% select(-matches("pw|pc"))

desire_lines_final$purpose[is.na(desire_lines_final$purpose)] = "commute"
desire_lines_final[is.na(desire_lines_final)] = 0

excess_active = desire_lines_final$walk_godutch + desire_lines_final$cycle_godutch - desire_lines_final$trimode_base
sel_excess = excess_active > 0
desire_lines_final$cycle_godutch[sel_excess] = desire_lines_final$cycle_godutch[sel_excess] - excess_active[sel_excess]

# prevent negative numbers cycling
sel_neg = desire_lines_final$cycle_godutch < 0
if(any(sel_neg)) {
  n_walk_base = desire_lines_final$walk_base[sel_neg]
  n_walk = desire_lines_final$walk_godutch[sel_neg]
  n_lengths = desire_lines_final$length[sel_neg]
  # increase cycling for greater distances while preventing reduction of walking
  n_walk_change_min = desire_lines_final$cycle_godutch[sel_neg]
  n_walk_change_max = n_walk_base - n_walk
  walk_coef = min(1 / (n_lengths / 750), 1) # for distances of 1.5 km, 50:50 walk:cycle
  n_walk_change = round(n_walk_change_max - (n_walk_change_max - n_walk_change_min) * walk_coef)
  desire_lines_final$walk_godutch[sel_neg] = desire_lines_final$walk_godutch[sel_neg] + n_walk_change
  desire_lines_final$cycle_godutch[sel_neg] = desire_lines_final$cycle_godutch[sel_neg] - n_walk_change
}

# calculate drive_godutch
desire_lines_final = desire_lines_final %>% 
  mutate(
    trimode_base = walk_base + cycle_base + drive_base,
    drive_godutch = trimode_base - (cycle_godutch + walk_godutch)
    ) 

# prevent negative driving values
sel_drive_neg = desire_lines_final$drive_godutch < 0
if(any(sel_drive_neg)) {
  desire_lines_final$cycle_godutch[sel_drive_neg] = 
    desire_lines_final$cycle_godutch[sel_drive_neg] +
    desire_lines_final$drive_godutch[sel_drive_neg]
  desire_lines_final$drive_godutch[sel_drive_neg] = 0
}

# sanity check percentages
# desire_lines_final %>%
#   sf::st_drop_geometry() %>% 
#   select(matches("base|godutch")) %>%
#   summarise_all(function(x) round(sum(x)/sum(desire_lines_final$trimode_base) * 100)) %>% 
#   tidyr::pivot_longer(cols = 1:8)

dsn = file.path(data_dir, site_name, "desire-lines-many.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(desire_lines_final, dsn = dsn)

# Get region of interest from desire lines --------------------------------
# this will now inevitably include the nearest town centre
min_flow_map = site_population / 80
desire_lines_busy = desire_lines_final %>% 
  filter(trimode_base >= min_flow_map)

convex_hull = sf::st_convex_hull(sf::st_union(desire_lines_busy))
study_area = stplanr::geo_buffer(convex_hull, dist = region_buffer_dist)
st_precision(study_area) = 1000000

to_add = site %>% 
  st_drop_geometry()
study_area = st_sf(cbind(to_add, study_area))

# zones_touching_small_study_area = zones_msoa_national[study_area, , op = sf::st_intersects]

dsn = file.path(data_dir, site_name, "small-study-area.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(study_area, dsn = dsn)

# Desire lines for small study area ---------------------------------------
desire_lines_few = desire_lines_final[study_area, , op = sf::st_within]

dsn = file.path(data_dir, site_name, "desire-lines-few.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(desire_lines_few, dsn = dsn)

# Get LSOA level JTS data for site ----------------------------------------
employ_site = st_intersection(jts0501, site)
employ_site$overlap_size = units::drop_units(st_area(employ_site))
access_employ = employ_site %>% 
  filter(overlap_size > 10000)
# mapview(access_employ)
# names(access_employ) = sub("X","", names(access_employ))

access_employ$weightedJobsPTt = apply(
  X = st_drop_geometry(access_employ[c("X100EmpPTt", "X500EmpPTt", "X5000EmpPTt")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_employ$weightedJobsCyct = apply(
  X = st_drop_geometry(access_employ[c("X100EmpCyct", "X500EmpCyct", "X5000EmpCyct")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_employ$weightedJobsCart = apply(
  X = st_drop_geometry(access_employ[c("X100EmpCart", "X500EmpCart", "X5000EmpCart")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_site = access_employ %>% 
  select(LSOA_code, weightedJobsPTt, weightedJobsCyct, weightedJobsCart)

j2 = jts0502 %>% 
  select(LSOA_code, PSPTt, PSCyct, PSCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j2)

j3 = jts0503 %>% 
  select(LSOA_code, SSPTt, SSCyct, SSCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j3)

j4 = jts0504 %>% 
  select(LSOA_code, FEPTt, FECyct, FECart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j4)

j5 = jts0505 %>% 
  select(LSOA_code, GPPTt, GPCyct, GPCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j5)

j6 = jts0506 %>% 
  select(LSOA_code, HospPTt, HospCyct, HospCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j6)

j7 = jts0507 %>% 
  select(LSOA_code, FoodPTt, FoodCyct, FoodCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j7)

j8 = jts0508 %>% 
  select(LSOA_code, TownPTt, TownCyct, TownCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j8)

access_means = access_site %>% 
  mutate(site_name = site_name) %>% 
  group_by(site_name) %>% 
  summarise(across(c(weightedJobsPTt:weightedJobsCart, PSPTt:TownCart), mean))

# site_all_base = sum(desire_lines_combined$all)
# site_walk_base = sum(desire_lines_combined$foot) / site_all_base
# site_cycle_base = sum(desire_lines_combined$bicycle) / site_all_base
# site_drive_base = sum(desire_lines_combined$car_driver) / site_all_base
# site_bus_base = sum(desire_lines_combined$bus) / site_all_base
# site_rail_base = (sum(desire_lines_combined$train) + sum(desire_lines_combined$light_rail)) / site_all_base
# site_other_base = (sum(desire_lines_combined$car_passenger) +sum(desire_lines_combined$taxi) + sum(desire_lines_combined$motorbike) + sum(desire_lines_combined$other)) / site_all_base

site_data = site
# site_data$pwalk = site_walk_base
# site_data$pcycle = site_cycle_base
# site_data$pdrive = site_drive_base
# site_data$pbus = site_bus_base
# site_data$prail = site_rail_base
# site_data$pother = site_other_base
# site_data = site_data %>% 
#   mutate(across(pwalk:pother, round, 2))

site_data = inner_join(site_data, st_drop_geometry(access_means), by = "site_name")
st_precision(site_data) = 1000000

dsn = file.path(data_dir, site_name, "site.geojson")
if(file.exists(dsn)) file.remove(dsn)
write_sf(site_data, dsn = dsn)


# JTS data for surrounding LSOAs ------------------------------------------
lsoa_c = st_centroid(jts0501)
lsoa_c = lsoa_c[large_study_area, ,op = sf::st_within]
lsoas_inside = filter(jts0501, LSOA_code %in% lsoa_c$LSOA_code)

lsoas_bounding = jts0501[desire_lines_bounding, ,op = sf::st_intersects]
lsoas_both = bind_rows(lsoas_inside, lsoas_bounding) %>% 
  unique()

lsoa_study_area = st_union(lsoas_both)
lsoa_study_area = sfheaders::sf_remove_holes(lsoa_study_area)
# lsoa_study_area2 = nngeo::st_remove_holes(lsoa_study_area)


lsoas_all = jts0501[lsoa_study_area, , op = sf::st_within]

# mapview(lsoas_all) + mapview(desire_lines_bounding)

lsoas_all$weightedJobsPTt = apply(
  X = st_drop_geometry(lsoas_all[c("X100EmpPTt", "X500EmpPTt", "X5000EmpPTt")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

lsoas_all$weightedJobsCyct = apply(
  X = st_drop_geometry(lsoas_all[c("X100EmpCyct", "X500EmpCyct", "X5000EmpCyct")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

lsoas_all$weightedJobsCart = apply(
  X = st_drop_geometry(lsoas_all[c("X100EmpCart", "X500EmpCart", "X5000EmpCart")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

lsoas_all = lsoas_all %>% 
  select(LSOA_code, weightedJobsPTt, weightedJobsCyct, weightedJobsCart)

j2 = jts0502 %>% 
  select(LSOA_code, PSPTt, PSCyct, PSCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j2)

j3 = jts0503 %>% 
  select(LSOA_code, SSPTt, SSCyct, SSCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j3)

j4 = jts0504 %>% 
  select(LSOA_code, FEPTt, FECyct, FECart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j4)

j5 = jts0505 %>% 
  select(LSOA_code, GPPTt, GPCyct, GPCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j5)

j6 = jts0506 %>% 
  select(LSOA_code, HospPTt, HospCyct, HospCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j6)

j7 = jts0507 %>% 
  select(LSOA_code, FoodPTt, FoodCyct, FoodCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j7)

j8 = jts0508 %>% 
  select(LSOA_code, TownPTt, TownCyct, TownCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j8)

st_precision(lsoas_all) = 1000000

dsn = file.path(data_dir, site_name, "jts-lsoas.geojson")
if(file.exists(dsn)) file.remove(dsn)
write_sf(lsoas_all, dsn = dsn)

# # check scenarios add up
# setwd("~/cyipt/actdev/")
# desire_lines = sf::read_sf("data-small/lcid/desire-lines-few.geojson")
# desire_lines_df = sf::st_drop_geometry(desire_lines)
# names(desire_lines_df)
# rowSums(desire_lines_df[7:9])
# rowSums(desire_lines_df[10:12])
