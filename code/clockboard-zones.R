library(tidyverse)
# remotes::install_github("zonebuilders/zonebuilder") 

# input data: we should probably have naming conventions for these
list.files(path)
site_area = sf::read_sf(file.path(path, "site.geojson"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-many.geojson"))	
routes_fast = readRDS(file.path(path, "routes_fast.Rds"))
# routes_balanced = readRDS(file.path(path, "routes_balanced.Rds"))
routes_quiet = readRDS(file.path(path, "routes_quiet.Rds"))
routes_walk = readRDS(file.path(path, "routes_walk.Rds"))

# zonebuilder zones -------------------------------------------------------
distances = c(0, zonebuilder::zb_100_triangular_numbers[1:9])
summary(cut(routes_fast$length / 1000, distances))

# colorspace::choose_palette()
# source("code/tests/color_palette.R")
site_centroid = site_area %>% 
  sf::st_centroid()
zones_concentric = zonebuilder::zb_zone(site_centroid, n_circles = 3)

# mapview::mapview(zones_concentric) +
#   routes_fast %>% sample_n(1000)

# Rename Go Dutch vars and joine with fast segs ---------------------------

routes_quiet_df = routes_quiet %>% 
  sf::st_drop_geometry() %>% 
  mutate(
    busyness = busynance / distances,
    dist_cycled_base = cycle_base * distances,
    dist_travelled_all = all_base * distances
    ) %>% 
  group_by(geo_code1, geo_code2) %>%
  summarise(
    # mean_gradient = weighted.mean(gradient_smooth, distances),
    # max_gradient = max(gradient_smooth),
    length_quiet = mean(length),
    busyness_mean_quiet = weighted.mean(busyness, distances),
    length_quiet = mean(length),
    diversion_factor_quiet = mean(length) / mean(crow_fly_distance),
    route_dist_cycled_quiet = mean(cycle_base) * mean(length)
    )

# Calculate Go Dutch cycling levels ---------------------------------------

routes_fast_df = routes_fast %>% 
  sf::st_drop_geometry() %>% 
  mutate(busyness = busynance / distances) %>% 
  group_by(geo_code1, geo_code2) %>%
  summarise(
    # mean_gradient = weighted.mean(gradient_smooth, distances),
    # max_gradient = max(gradient_smooth),
    length_fast = mean(length),
    busyness_mean = weighted.mean(busyness, distances), 
    busyness_90th = quantile(x = busyness, probs = 0.9),
    busyness_max = max(busyness),
    pcycle_godutch_uptake = pct::uptake_pct_godutch_2020(distance = mean(length), gradient = weighted.mean(gradient_smooth, distances)),
    cycle_godutch_additional = pcycle_godutch_uptake * mean(drive_base),
    cycle_godutch = mean(cycle_base) + cycle_godutch_additional,
    pcycle_godutch = cycle_godutch / mean(all_base),
    drive_godutch = mean(drive_base) - cycle_godutch_additional, # ensure totals add up
    diversion_factor = mean(length) / mean(crow_fly_distance),
    route_dist_cycled_base = mean(cycle_base) * mean(length),
    route_dist_cycled_dutch = mean(cycle_godutch) * mean(length)
  )

# # sanity tests
# summary(routes_fast_df)
# nrow(routes_fast_df)
# mean(routes_fast_df$busyness_90th)
# mean(routes_fast_df$busyness_max)
# mean(routes_fast_df$route_dist_cycled_base)
# mean(routes_fast_df$route_dist_cycled_dutch)

routes_fast_joined = left_join(routes_fast, routes_fast_df)
routes_fast_joined2 = left_join(routes_fast_joined, routes_quiet_df)
summary(routes_fast_joined)
summary(routes_fast_joined2)

# routes_fast_broken = stplanr::line_segment(routes_fast, segment_length = 500) # fails
routes_fast_cents = routes_fast_joined2 %>%
  sf::st_centroid() %>% 
  sf::st_join(zones_concentric[1])
nms = grep(pattern = "route_dist", x = names(routes_fast_cents))
for(i in nms) {
  dc = routes_fast_cents[[i]]
  dc[dc == 0] = 1
  routes_fast_cents[[i]] = dc
}

summary(routes_fast_cents)

zone_df = routes_fast_cents %>% 
  sf::st_drop_geometry() %>% 
  group_by(label) %>% 
  summarise(
    busyness_cycle_base = weighted.mean(busyness_mean, route_dist_cycled_base),
    busyness_cycle_dutch = weighted.mean(busyness_mean, route_dist_cycled_dutch),
    busyness_cycle_base_quiet = weighted.mean(busyness_mean_quiet, route_dist_cycled_quiet),
    circuity_cycle_fast = weighted.mean(diversion_factor, route_dist_cycled_base),
    # circuity_cycle_balanced = NA,
    circuity_cycle_quiet = weighted.mean(diversion_factor_quiet, route_dist_cycled_quiet),
    quietness_diversion = circuity_cycle_quiet / circuity_cycle_fast
  )

summary(zone_df)
zones_db = left_join(zones_concentric, zone_df)
names(zones_db)
summary(zones_db)

# Cirquity walking --------------------------------------------------------
routes_walk
routes_walk$walk_base[routes_walk$walk_base == 0] = 1
routes_walk_joined = inner_join(
  routes_walk,
  desire_lines %>% sf::st_drop_geometry() %>% select(matches("geo_code"), length)
) %>%
  mutate(circuity = distance / length)
mapview::mapview(routes_walk_joined["circuity"])
#
routes_walk_split = stplanr::line_breakup(l = routes_walk_joined, z = zones_concentric)
routes_walk_split$length_segment = stplanr::geo_length(routes_walk_split)
mapview::mapview(routes_walk_split["circuity"]) + zones_concentric
routes_walk_diversion = routes_walk_split %>%
  sf::st_centroid() %>%
  # idea: add total trips or godutch in Phase II
  mutate(walk_distance_base = walk_base * length_segment) %>%
  select(circuity, walk_distance_base)

zone_df_walk = sf::st_join(
  routes_walk_diversion,
  zones_concentric %>% select(label)
)
plot(zone_df_walk)
mapview::mapview(zone_df_walk) + zones_concentric

zone_df_to_join = zone_df_walk %>%
  sf::st_drop_geometry() %>%
  group_by(label) %>%
  summarise(circuity_walk = weighted.mean(circuity, walk_distance_base))

# # EXCLUDED WALKING ROUTES BECAUSE OF BUG THAT MEANS `ROUTES_WALK_JOINED` HAS NO CONTENTS
zones_db = left_join(zones_db, zone_df_to_join)
#mapview::mapview(zones_db["circuity_walk"]) + routes_walk_diversion

# mapview::mapview(zones_db["busyness_cycle_base"]) +
#   mapview::mapview(routes_fast_cents)
# mapview::mapview(zones_db["busyness_cycle_dutch"])
# mapview::mapview(zones_db["quietness_diversion"])
# plot(zones_db[-c(1:3)])

sf::st_precision(zones_db) = 10000
zones_db = zones_db %>% 
  mutate_if(is.numeric, function(x) round(x, digits = 2))


# Create 'minimap infographic' --------------------------------------------

# todo (maybe in Phase II): create minimap visual

db_file = file.path(path, "dartboard.geojson")
if(file.exists(db_file)) file.remove(db_file)
sf::write_sf(zones_db, db_file)

# head(readLines("data-small/great-kneighton/dartboard.geojson"))

