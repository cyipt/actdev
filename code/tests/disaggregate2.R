# Aim: demonstrate disaggregating polygons for #24

# remotes::install_github("itsleeds/od", "bdb44597f0b701e683e5208b837c9a91c7036838")
remotes::install_github("itsleeds/od")
remotes::install_github("ITSLeeds/pct")
library(dplyr)

if(!exists("site_name")) site_name = "chapelford"
sites = sf::read_sf("data-small/all-sites.geojson")
site = sites[sites$site_name == site_name, ]
path = file.path("data-small", site_name)

# Go to the repo's root directory, assuming we're executing the script from inside its directory.
if(!dir.exists("data-small")) {
  setwd("../..")
}

# input data: we should probably have naming conventions for these
site_area = sf::read_sf(file.path(path, "site.geojson"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-few.geojson"))	# TODO or many?
study_area = sf::read_sf(file.path(path, "small-study-area.geojson"))	# TODO or large?
# buildings = osmextract::oe_get(study_area, layer = "multipolygons")
osm_polygons = osmextract::oe_get(sf::st_centroid(study_area), layer = "multipolygons")
# mapview::mapview(study_area) +
#   mapview::mapview(osmextract::geofabrik_zones)

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
pct_zone = pct::pct_regions[site_area %>% sf::st_centroid(), ]
zones = pct::get_pct_zones(pct_zone$region_name)
zones = pct::get_pct_zones(pct_zone$region_name, geography = "msoa")
zones_of_interest = zones[zones$geo_code %in% c(desire_lines$geo_code1, desire_lines$geo_code2), ]
buildings_in_zones = osm_buildigs[zones_of_interest, , op = sf::st_within]

if(site_name == "chapelford") {
  u = "http://abstreet.s3-website.us-east-2.amazonaws.com/dev/data/input/cheshire/procgen_houses.json.gz"
  download.file(u, "procgen_houses.json.gz")
  system("gunzip procgen_houses.json.gz")
  procgen_houses = sf::read_sf("procgen_houses.json")
}

mapview::mapview(zones_of_interest) +
  mapview::mapview(buildings_in_zones)
buildings_in_zones = buildings_in_zones %>%
  filter(!is.na(osm_way_id)) %>%
  select(osm_way_id, building)


osm_polygons_in_site = osm_polygons[site_area, , op = sf::st_within]
osm_polygons_resi_site = osm_polygons_in_site %>%
  filter(building == "residential") %>% 
  select(osm_way_id, building)

if(exists("procgen_houses")) {
  procgen_site = procgen_houses[site_area, , op = sf::st_within]
  procgen_osm = sf::st_sf(
    data.frame(
      osm_way_id = rep(NA, nrow(procgen_site)),
      building = rep(NA, nrow(procgen_site))
    ), 
    geometry = procgen_site$geometry
  )
  osm_polygons_resi_site = rbind(osm_polygons_resi_site, procgen_osm)
}

mapview::mapview(osm_polygons_resi_site)

# loop over each desire line
i = 1
for(i in seq(nrow(desire_lines))) {
  pop = desire_lines$all_base[i]
  origins = osm_polygons_resi_site %>% sample_n(size = pop)
  destination_zone = zones_of_interest %>% filter(geo_code == desire_lines$geo_code2[i])
  destination_buildings = buildings_in_zones[destination_zone, , op = sf::st_within]
  destinations = destination_buildings %>% sample_n(size = pop, replace = TRUE)
  origin_coords = origins %>% sf::st_centroid() %>% sf::st_coordinates()
  destination_coords = destinations %>% sf::st_centroid() %>% sf::st_coordinates()
  desire_lines_disag = od::odc_to_sf(odc = cbind(origin_coords, destination_coords))
  # mapview::mapview(desire_lines_disag) + mapview::mapview(destination_zone)
  desire_lines_disag$mode_baseline = desire_lines_disag$mode_godutch = NA
  n_walk = desire_lines$walk_base[i]
  desire_lines_disag$mode_baseline[sample(nrow(desire_lines_disag), size = n_walk)] = "walk"
  no_mode = which(is.na(desire_lines_disag$mode_baseline))
  desire_lines_disag$mode_baseline[sample(no_mode, size = desire_lines$cycle_base[i])] = "cycle"
  no_mode = which(is.na(desire_lines_disag$mode_baseline))
  desire_lines_disag$mode_baseline[sample(no_mode, size = desire_lines$drive_base[i])] = "drive"
  desire_lines_disag$mode_baseline[is.na(desire_lines_disag$mode_baseline)] = "other"
  if(i == 1) {
    desire_lines_out = desire_lines_disag
  } else {
    desire_lines_out = rbind(desire_lines_out, desire_lines_disag)
  }
    
  # table(desire_lines_disag$mode_baseline)
  # desire_lines[i, ]
}



mapview::mapview(desire_lines_out)
