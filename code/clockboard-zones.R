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

# routes_fast_broken = stplanr::line_segment(routes_fast, segment_length = 500) # fails
routes_fast_seg = routes_fast %>% sf::st_cast("LINESTRING")
routes_fast_seg$segment_length = stplanr::geo_length(routes_fast_seg)
routes_fast_cents = routes_fast_seg %>%
  sf::st_centroid() %>% 
  mutate(
    dist_cycled_base = cycle_base * segment_length,
    dist_cycled_dutch = cycle_godutch * segment_length,
    ) %>% 
  sf::st_join(zones_concentric[1])

zone_df = routes_fast_cents %>% 
  sf::st_drop_geometry() %>% 
  group_by(label) %>% 
  summarise(
    busyness_cycle_base = weighted.mean(mean_busyness, dist_cycled_base),
    busyness_cycle_dutch = weighted.mean(mean_busyness, dist_cycled_dutch),
    circuity_cycle_fast = NA,
    circuity_cycle_balanced = NA,
    circuity_cycle_quiet = NA,
    circuity_walk = NA,
    quietness_diversion = NA
  )

zones_db = left_join(zones_concentric, zone_df)
names(zones_db)

mapview::mapview(zones_db["busyness_cycle_base"])

sf::st_precision(zones_db) = 10000

db_file = file.path(path, "dartboard.geojson")
if(file.exists(db_file)) file.remove(db_file)
sf::write_sf(zones_db, db_file)


# head(readLines("data-small/great-kneighton/dartboard.geojson"))