# Aim: demonstrate disaggregating polygons for #24

# remotes::install_github("itsleeds/od", "bdb44597f0b701e683e5208b837c9a91c7036838")
remotes::install_github("itsleeds/od")
remotes::install_github("ITSLeeds/pct")
remotes::install_github("a-b-street/abstr")
library(dplyr)

if(!exists("site_name")) site_name = "chapelford"
sites = sf::read_sf("data-small/all-sites.geojson")
site = sites[sites$site_name == site_name, ]
path = file.path("data-small", site_name)

# input data: we should probably have naming conventions for these
site_area = sf::read_sf(file.path(path, "site.geojson"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-few.geojson"))	
study_area = sf::read_sf(file.path(path, "small-study-area.geojson"))
# buildings = osmextract::oe_get(study_area, layer = "multipolygons")
osm_polygons = osmextract::oe_get(sf::st_centroid(study_area), layer = "multipolygons")

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

n_buildings_per_zone = aggregate(buildings_in_zones, zones_of_interest, FUN = "length")
if(anyNA(n_buildings_per_zone$osm_way_id)) {
  zones_lacking_buildings = which(is.na(n_buildings_per_zone$osm_way_id))
  new_buildings = sf::st_sample(zones_of_interest[zones_lacking_buildings, ], size = 5 * length(zones_lacking_buildings))
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
if(n_houses < n_dwellings_site) {
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

if(exists("procgen_houses")) {
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

abstr_list = abstr::ab_scenario(houses, buildings = buildings_in_zones, desire_lines = desire_lines, zones = zones_of_interest, output_format = "json_list")
abstr::ab_save(abstr_list, file.path(path, "scenario.json"))
# file.edit(file.path(path, "scenario.json"))
