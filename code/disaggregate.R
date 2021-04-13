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
trip_attractors = sf::read_sf(file.path(path, "trip_attractors.geojson"))
houses = sf::read_sf(file.path(path, "site_buildings.geojson"))
houses_in_site = houses[site, , op = sf::st_within]

if(nrow(houses_in_site) == 0) {
  message("No houses in site, sampling them.")
  n_houses_to_generate = 20
  new_house_centroids = sf::st_sample(site, size = n_houses_to_generate)
  new_house_polys = stplanr::geo_buffer(new_house_centroids, dist = 8, nQuadSegs = 1)
  plot(new_house_polys)
  new_houses = sf::st_sf(
    data.frame(osm_way_id = rep(NA, n_houses_to_generate), building = "residential"),
    geometry = new_house_polys
  )
  houses = rbind(houses, new_houses)
} 
# mapview::mapview(houses) + mapview::mapview(site) # check houses in site
# mapview::mapview(houses_in_site) + mapview::mapview(site) # check houses in site
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
  d$osm_way_id = paste0("synthetic", 1:length(p_sample))
  b = sf::st_sf(d, geometry = stplanr::geo_buffer(p_sample, dist = 50))
  sz = rbind(sz, b)
}

# mapview::mapview(sz) + mapview::mapview(zones_with_buildings) +
# mapview::mapview(zones_without_buildings) +
# mapview::mapview(desire_lines_many)
# mapview::mapview(desire_lines_disag)

# Route to random points:
# desire_lines_disag = od_disaggregate(od = desire_lines_many_min, z = z, subpoints = sp, population_per_od = p)
# Route to buildings:
desire_lines_disag = od_disaggregate(od = desire_lines_many_min, z = z, subzones = sz, population_per_od = p)

desire_lines_many = desire_lines_disag %>% 
  mutate(
    pwalk_base = walk_base/trimode_base,
    pcycle_base = cycle_base/trimode_base,
    pdrive_base = drive_base/trimode_base
  ) 
desire_lines_many$length = stplanr::geo_length(desire_lines_many)
names(desire_lines_many)[1:2] = names(od_site)[1:2]

# # sanity tests  
# sum(desire_lines_many$all_base) == sum(desire_lines_disag$all_base)
# sum(desire_lines_many$walk_base) == sum(desire_lines_disag$walk_base)
# library(tmap)
# tmap_mode("view")
# qtm(desire_lines_disag) + qtm(site)
