# Aim: demonstrate disaggregating polygons for #24

# remotes::install_github("itsleeds/od", "bdb44597f0b701e683e5208b837c9a91c7036838")
remotes::install_github("itsleeds/od")
library(dplyr)

setwd("~/cyipt/actdev/")
# input data: we should probably have naming conventions for these
sites = sf::read_sf("all-sites.geojson")
site_area = sf::read_sf("geojsons/great-kneighton.geojson")
desire_lines = sf::read_sf("data-small/great-kneighton/great-kneighton-desire-lines.geojson")
study_area = sf::read_sf("data-small/great-kneighton/great-kneighton-study-area.geojson")
# buildings = osmextract::oe_get(study_area, layer = "multipolygons")
osm_polygons = osmextract::oe_get("cambridgeshire", layer = "multipolygons")

# from od/data-raw folder
building_types = c(
  "office",
  "industrial",
  "commercial",
  "retail",
  "warehouse",
  "civic",
  "public"
)
osm_buildigs = osm_polygons %>%
  filter(building %in% building_types)
zones = pct::get_pct_zones("cambridgeshire")
zones = pct::get_pct_zones("cambridgeshire", geography = "msoa")
zones_of_interest = zones[zones$geo_code %in% c(desire_lines$geo_code1, desire_lines$geo_code2), ]
mapview::mapview(zones_of_interest)
buildings_in_zones = osm_buildigs[zones_of_interest, , op = sf::st_within]
mapview::mapview(buildings_in_zones)
buildings_in_zones = buildings_in_zones %>%
  filter(!is.na(osm_way_id)) %>%
  select(osm_way_id, building)
osm_polygons_in_site = osm_polygons[site_area, , op = sf::st_within]
osm_polygons_resi_site = osm_polygons_in_site %>%
  filter(building == "residential") %>% 
  select(osm_way_id, building)
  
buildings_od = rbind(buildings_in_zones, osm_polygons_resi_site)
mapview::mapview(buildings_od) +
  mapview::mapview(site_area) +
  mapview::mapview(desire_lines) +
  mapview::mapview(zones_of_interest)
od_df = desire_lines %>%
  sf::st_drop_geometry()
od_df
zones_of_interest
zones_of_interest_min = zones_of_interest %>%
  select(geo_code)
site_area
site_area_id = sf::st_sf(data.frame(geo_code = "s1"), geometry = site_area$geometry)
zones_all = rbind(zones_of_interest_min, site_area_id)
od_df$geo_code1 = "s1"

desire_lines_disag = od::od_disaggregate(od = od_df, z = zones_all, subzones = buildings_od) 
desire_lines_disag = desire_lines_disag %>% 
  select(car_driver, bicycle, foot, car_commute_godutch, bicycle_commute_godutch, walk_commute_godutch)
summary(desire_lines_disag)
sum(desire_lines$car_driver)
sum(desire_lines_disag$car_driver)
sum(desire_lines_disag$car_commute_godutch)

# implement scenarios while keeping flow totals unchanged
change_walking = sum(desire_lines$walk_commute_godutch) - sum(desire_lines$foot)
change_cycling = sum(desire_lines$bicycle_commute_godutch) - sum(desire_lines$bicycle)
change_driving = sum(desire_lines$car_commute_godutch) - sum(desire_lines$car_driver)
desire_lines_disag$car_commute_godutch = desire_lines_disag$car_driver
desire_lines_disag$bicycle_commute_godutch = desire_lines_disag$bicycle
desire_lines_disag$walk_commute_godutch = desire_lines_disag$foot

desire_lines_disag$distance = stplanr::geo_length(desire_lines_disag)
desire_lines_disag$prob_switch = pct::uptake_pct_godutch_2020(distance = desire_lines_disag$distance, gradient = 0)
plot(desire_lines_disag$distance, desire_lines_disag$prob_switch)
desire_lines_disag$prob_switch[desire_lines_disag$car_driver < 1] = 0
sel_car_to_bike = sample(nrow(desire_lines_disag), size = change_cycling, prob = desire_lines_disag$prob_switch)
desire_lines_disag$car_commute_godutch[sel_car_to_bike] = desire_lines_disag$car_driver[sel_car_to_bike] - 1
desire_lines_disag$bicycle_commute_godutch[sel_car_to_bike] = desire_lines_disag$bicycle[sel_car_to_bike] + 1

rowSums(desire_lines_disag[1:3] %>% sf::st_drop_geometry()) ==
rowSums(desire_lines_disag[4:6] %>% sf::st_drop_geometry())

mapview::mapview(desire_lines_disag) + mapview::mapview(buildings_od)

file.remove("data-small/great-kneighton/desire_lines_disag.geojson")
sf::write_sf(desire_lines_disag, "data-small/great-kneighton/desire_lines_disag.geojson")
sf::write_sf(buildings_od, "data-small/great-kneighton/buildings_od.geojson")
