# Aim: demonstrate disaggregating polygons for #24

# remotes::install_github("itsleeds/od", "bdb44597f0b701e683e5208b837c9a91c7036838")
remotes::install_github("itsleeds/od")
remotes::install_github("ITSLeeds/pct")
remotes::install_github("a-b-street/abstr")
library(dplyr)

if(!exists("site_name")) {
  site_name = "ashton-park"
} 
if(!exists("sites")) {
  sites = sf::read_sf("data-small/all-sites.geojson")
} 
site = sites[sites$site_name == site_name, ]
path = file.path("data-small", site_name)

# input data: we should probably have naming conventions for these
site_area = sf::read_sf(file.path(path, "site.geojson"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-few.geojson"))	
study_area = sf::read_sf(file.path(path, "small-study-area.geojson"))
# buildings = osmextract::oe_get(study_area, layer = "multipolygons")
osm_polygons = osmextract::oe_get(sf::st_centroid(study_area), layer = "multipolygons")

# get procedurally generated houses
# https://github.com/cyipt/actdev/issues/81
procgen_url = paste0(
  "http://abstreet.s3-website.us-east-2.amazonaws.com/dev/data/input/",
  gsub(pattern = "-", replacement = "_", x = site_name),
  "/procgen_houses.json.gz"
)
procgen_path = file.path(path, "procgen_houses.json")
procgen_path_gz = file.path(path, "procgen_houses.json.gz")

procgen_get = httr::GET(
  url = procgen_url,
  httr::write_disk(procgen_path_gz, overwrite = TRUE)
  )
procgen_exists = httr::status_code(procgen_get) != 404

# # # sanity check scenario data
# class(desire_lines)
# sum(desire_lines$trimode_base)
# sum(desire_lines$walk_base, desire_lines$cycle_base, desire_lines$drive_base)
# sum(desire_lines$walk_godutch, desire_lines$cycle_godutch, desire_lines$drive_godutch)

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
zones = pct::get_pct_zones(pct_zone$region_name, geography = "msoa")
zones_of_interest = zones[zones$geo_code %in% c(desire_lines$geo_code1, desire_lines$geo_code2), ]

# add town zone, see #74
zone_town = zones %>% 
  sf::st_drop_geometry() %>% 
  slice(1) %>% 
  mutate_all(function(x) NA) %>% 
  mutate(geo_code = tail(desire_lines$geo_code2, 1)) 
zone_town_geometry = lwgeom::st_endpoint(tail(desire_lines, 1)) %>% 
  stplanr::geo_buffer(dist = 1000) 
zone_town_sf = sf::st_sf(zone_town, geometry = zone_town_geometry)
zones_of_interest = rbind(zones_of_interest, zone_town_sf)

buildings_in_zones = osm_buildigs[zones_of_interest, , op = sf::st_within]

if(procgen_exists) {
  file.remove(procgen_path)
  system(paste0("gunzip ", procgen_path_gz))
  procgen_houses = sf::read_sf(procgen_path)
}

mapview::mapview(zones_of_interest) +
  mapview::mapview(buildings_in_zones)
buildings_in_zones = buildings_in_zones %>%
  filter(!is.na(osm_way_id)) %>%
  select(osm_way_id, building)

n_buildings_per_zone = aggregate(buildings_in_zones, zones_of_interest, FUN = "length")
summary(n_buildings_per_zone$osm_way_id)
mbz = 10
zones_lacking_buildings = n_buildings_per_zone$osm_way_id < mbz
zones_lacking_buildings[is.na(zones_lacking_buildings)] = TRUE
if(any(zones_lacking_buildings)) {
  sz = rep(5, length(zones_lacking_buildings) ) # n buildings per zone - arbitrary
  new_buildings = sf::st_sample(zones_of_interest[zones_lacking_buildings, ], size = sz)
  new_buildings = sf::st_sf(
    data.frame(osm_way_id = rep(NA, length(new_buildings)), building = NA),
    geometry = stplanr::geo_buffer(new_buildings, dist = 20, nQuadSegs = 1)
  )
  buildings_in_zones = rbind(buildings_in_zones, new_buildings)
}

osm_polygons_in_site = osm_polygons[site_area, , op = sf::st_within]
houses = osm_polygons_in_site %>%
  filter(building == "residential") %>% 
  select(osm_way_id, building)
n_houses = nrow(houses)
n_dwellings_site = site$dwellings_when_complete
if(n_houses < n_dwellings_site && !procgen_exists) {
  n_houses_to_generate = n_dwellings_site - n_houses
  new_house_centroids = sf::st_sample(site_area, size = n_houses_to_generate)
  new_house_polys = stplanr::geo_buffer(new_house_centroids, dist = 8, nQuadSegs = 1)
  plot(new_house_polys)
  new_houses = sf::st_sf(
    data.frame(osm_way_id = rep(NA, n_houses_to_generate), building = "residential"),
    geometry = new_house_polys
    )
  houses = rbind(houses, new_houses)
}

if(procgen_exists) {
  # quick fix for https://github.com/cyipt/actdev/issues/82
  # todo: update when new procedurally generated houses are available
  site_area = stplanr::geo_buffer(site_area, dist = 250) # expand boundary for #82
  procgen_site = procgen_houses[site_area, , op = sf::st_within]
  procgen_osm = sf::st_sf(
    data.frame(
      osm_way_id = rep(NA, nrow(procgen_site)),
      building = rep(NA, nrow(procgen_site))
    ), 
    geometry = procgen_site$geometry
  )
  houses = rbind(houses, procgen_osm)
}

mapview::mapview(procgen_houses) +
  mapview::mapview(site)

abstr_base = abstr::ab_scenario(
  houses,
  buildings = buildings_in_zones,
  desire_lines = desire_lines,
  zones = zones_of_interest,
  scenario = "base",
  output_format = "json_list"
)

names(desire_lines)

abstr_godutch = abstr::ab_scenario(
  houses,
  buildings = buildings_in_zones,
  desire_lines = desire_lines,
  zones = zones_of_interest,
  scenario = "godutch",
  output_format = "json_list"
)

abstr::ab_save(abstr_godutch, file.path(path, "scenario-godutch.json"))
abstr::ab_save(abstr_base, file.path(path, "scenario-base.json"))
# file.edit(file.path(path, "scenario.json"))


# debugging / sanity checks:
abstr_godutch_sf = abstr::ab_scenario(
  houses,
  buildings = buildings_in_zones,
  desire_lines = desire_lines,
  zones = zones_of_interest,
  scenario = "godutch",
  output_format = "sf"
)

mapview::mapview(abstr_godutch_sf %>% sample_n(20)) +
  mapview::mapview(houses)

# abstr_godutch = abstr::ab_scenario(
#   houses,
#   buildings = buildings_in_zones,
#   desire_lines = desire_lines %>% slice(1:5),
#   zones = zones_of_interest,
#   scenario = "godutch",
#   output_format = "json_list"
# )
# str(abstr_godutch)
