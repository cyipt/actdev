library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

sites_join = sf::read_sf("data-small/all-sites.geojson")

# remove sites with less than 500 dwellings at completion
sites_table_geo = sites_join %>% 
  filter(dwellings_when_complete >= 500)

sites_table_geo %>% 
  skimr::skim()
sites_table_point = sf::st_centroid(sites_table_geo)
# works but no legend:
tm_shape(sites_table_point) +
  tm_dots(size = "dwellings_when_complete") 

regions = sf::read_sf("https://github.com/saferactive/saferactive/releases/download/0.1.4/Regions_.December_2020._EN_BGC.geojson")
# gb_outline = sf::read_sf("https://github.com/martinjc/UK-GeoJSON/raw/master/json/administrative/gb/lad.json")
gb_outline = rnaturalearth::ne_countries(country = "United Kingdom", scale = "medium")
tmap_mode("plot")
tm_shape(regions) +
  tm_borders() +
tm_shape(sites_table_point, bbox = tmaptools::bb(sites_table_point, ext = 1.5)) +
  tm_dots(size = "dwellings_when_complete") +
  tm_shape(gb_outline) +
  tm_borders(lwd = 2)




sites_table = sites_table_geo %>% 
  select(percent_commute_active_base, percent_commute_walk_base, percent_commute_cycle_base, percent_commute_drive_base, circuity_fast_cycle, circuity_walk, busyness_fast_cycle, median_commute_distance, distance_to_town, percent_commute_active_scenario, percent_commute_walk_scenario, percent_commute_cycle_scenario, percent_commute_drive_scenario, in_site_walk_circuity, in_site_cycle_circuity, in_site_drive_circuity)
st_drop_geometry() 



sites_table$in_site_walk_circuity = as.numeric(sites_table$in_site_walk_circuity)
sites_table$in_site_cycle_circuity = as.numeric(sites_table$in_site_cycle_circuity)
sites_table$in_site_drive_circuity = as.numeric(sites_table$in_site_drive_circuity)

mm = sapply(sites_table, mean, na.rm = TRUE)
minm = sapply(sites_table, min, na.rm = TRUE)
maxm = sapply(sites_table, max, na.rm = TRUE)

metrics = names(sites_table)

end_table = tibble(metrics, mm, minm, maxm)

## note no walk circuity results for wynyard and long marston because there are no walk commute routes

# robustbase::colMedians(sites_join, na.rm = FALSE)

# code to get site metrics
i = sites_join$site_name[1]
sites_join$percent_commute_walk_base = NA
sites_join$percent_commute_cycle_base = NA
sites_join$percent_commute_drive_base = NA
sites_join$percent_commute_bus_base = NA
sites_join$percent_commute_rail_base = NA
sites_join$percent_commute_other_base = NA
sites_join$median_commute_distance = NA
sites_join$distance_to_town = NA
sites_join$percent_commute_active_base = NA
sites_join$percent_drive_convertable = NA
sites_join$percent_mapped_drive_convertable = NA
# sites_join$percent_scenario_active = NA
sites_join$percent_commute_active_scenario = NA
sites_join$percent_commute_drive_scenario = NA
sites_join$percent_commute_walk_scenario = NA
sites_join$percent_commute_cycle_scenario = NA
sites_join$crossing_points = NA
for(i in sites_join$site_name) {
  f = paste0("data-small/", i, "/desire-lines-many.geojson")
  desire_lines = sf::read_sf(f)
  desire_lines = desire_lines %>% filter(purpose == "commute") # take out journeys to towns
  f = paste0("data-small/", i, "/all-census-od.csv")
  all_desire_lines = read_csv(f)
  site_boundary = sites_join$geometry[sites_join$site_name == i]
  site_line = st_cast(site_boundary,"LINESTRING")
  f = paste0("data-small/", i, "/routes-fast.geojson")
  fast_routes = sf::read_sf(f)
  route_town = fast_routes %>% filter(purpose == "town")
  
  crossing_points = st_intersection(fast_routes, site_line)
  # prop_near = sum(desire_lines$all_base) / sum(all_desire_lines$all) #proportion of commutes that are represented in desire_lines_many 
  median_dist = round(weighted.median(all_desire_lines$length, w = all_desire_lines$all) / 1000, 1)
  all_trips = sum(all_desire_lines$all)
  drive_trips = sum(all_desire_lines$car_driver)
  active_base = sum(all_desire_lines$foot) + sum(all_desire_lines$bicycle)
  # walk_base = sum(all_desire_lines$foot)
  # cycle_base = sum(all_desire_lines$bicycle)
  percent_trimode_trips = sum(all_desire_lines$trimode_base) / all_trips
  
  percent_commute_walk_base = round(100 * sum(all_desire_lines$foot) / all_trips)
  percent_commute_cycle_base = round(100 * sum(all_desire_lines$bicycle) / all_trips)
  percent_commute_drive_base = round(100 * sum(all_desire_lines$car_driver) / all_trips)
  
  percent_commute_bus_base = round(100 * sum(all_desire_lines$bus) / all_trips)
  percent_commute_rail_base = round(100 * ((sum(all_desire_lines$train) + sum(all_desire_lines$light_rail)) / all_trips))
  percent_commute_other_base = round(100 * ((sum(all_desire_lines$car_passenger) + sum(all_desire_lines$taxi) + sum(all_desire_lines$motorbike) + sum(all_desire_lines$other)) / all_trips))
  
  drive_near = sum(desire_lines$drive_base)
  drive_dutch = sum(desire_lines$drive_godutch)
  active_near = sum(desire_lines$walk_base) + sum(desire_lines$cycle_base)
  active_dutch = sum(desire_lines$walk_godutch) + sum(desire_lines$cycle_godutch)
  # walk_near = sum(desire_lines$walk_base)
  walk_dutch = sum(desire_lines$walk_godutch)
  # cycle_near = sum(desire_lines$cycle_base)
  cycle_dutch = sum(desire_lines$cycle_godutch)
  
  pchanged = round(100 * (drive_near - drive_dutch) / drive_trips)  
  pchanged_ofnear = round(100 * (drive_near - drive_dutch) / drive_near)
  percent_commute_active_base = round(100 * active_base / all_trips)
  # percent_scenario_active = round(100 * (active_base + active_dutch - active_near) / all_trips)
  # to correct for missing desire lines in small sites: calculate the % by which active travel has increased in the mapped desire lines, then assume it increases by the same proportion in unmapped desire lines (yes this is slightly optimistic because the unmapped ones will be longer, but at least it evens things out between sites with different populations) 
  percent_commute_active_increase = active_dutch / active_near
  percent_commute_active_scenario = (active_base / all_trips * percent_commute_active_increase)
  percent_commute_drive_scenario = round(100 * (percent_trimode_trips - percent_commute_active_scenario))
  percent_commute_walk_scenario = round(100 * (walk_dutch / active_dutch * percent_commute_active_scenario))
  percent_commute_cycle_scenario = round(100 * (cycle_dutch / active_dutch * percent_commute_active_scenario))
  
  # code to re-add the data to the sites_join table
  sites_join$percent_commute_walk_base[sites_join$site_name == i] = percent_commute_walk_base
  sites_join$percent_commute_cycle_base[sites_join$site_name == i] = percent_commute_cycle_base
  sites_join$percent_commute_drive_base[sites_join$site_name == i] = percent_commute_drive_base
  sites_join$percent_commute_bus_base[sites_join$site_name == i] = percent_commute_bus_base
  sites_join$percent_commute_rail_base[sites_join$site_name == i] = percent_commute_rail_base
  sites_join$percent_commute_other_base[sites_join$site_name == i] = percent_commute_other_base
  
  sites_join$distance_to_town[sites_join$site_name == i] = round(route_town$length / 1000, 1)
  sites_join$median_commute_distance[sites_join$site_name == i] = median_dist
  sites_join$percent_commute_active_base[sites_join$site_name == i] = percent_commute_active_base
  sites_join$percent_drive_convertable[sites_join$site_name == i] = pchanged
  sites_join$percent_mapped_drive_convertable[sites_join$site_name == i] = pchanged_ofnear
  # sites_join$percent_scenario_active[sites_join$site_name == i] = percent_scenario_active
  sites_join$percent_commute_active_scenario[sites_join$site_name == i] = round(100 * percent_commute_active_scenario)
  sites_join$percent_commute_drive_scenario[sites_join$site_name == i] = percent_commute_drive_scenario
  sites_join$percent_commute_walk_scenario[sites_join$site_name == i] = percent_commute_walk_scenario
  sites_join$percent_commute_cycle_scenario[sites_join$site_name == i] = percent_commute_cycle_scenario
  sites_join$crossing_points[sites_join$site_name == i] = length(unique(crossing_points$geometry))
  # message(round(100 * (drive_trips - drive_dutch) / drive_trips), " percent in ", i)
  
}

# add in circuity measures
sites_join$busyness_fast_cycle = NA
sites_join$circuity_fast_cycle = NA
sites_join$circuity_walk = NA

for(i in sites_join$site_name) {
  f = paste0("data-small/", i, "/desire-lines-many.geojson")
  desire_lines = sf::read_sf(f)
  f = paste0("data-small/", i, "/routes-fast.geojson")
  fast_routes = sf::read_sf(f)
  f = paste0("data-small/", i, "/routes-walk.geojson")
  
  if(file.exists(f)) { walk_routes = sf::read_sf(f)
  walk = walk_routes %>% st_drop_geometry %>% select(geo_code2, route_length = distance, walk_base)
  join_walk = inner_join(walk, desire_lines %>% select(geo_code2, euclidean_length = length), by = "geo_code2")
  join_walk$circuity = join_walk$route_length / join_walk$euclidean_length
  walk_circuity = round(weighted.mean(join_walk$circuity, w = join_walk$walk_base), 2)
  sites_join$circuity_walk[sites_join$site_name == i] = walk_circuity
  }
  
  fast = fast_routes %>% st_drop_geometry %>% select(geo_code2, route_length = length, cycle_base)
  join_fast = inner_join(fast, desire_lines %>% select(geo_code2, euclidean_length = length), by = "geo_code2")
  join_fast$circuity = join_fast$route_length / join_fast$euclidean_length
  fast_circuity = round(weighted.mean(join_fast$circuity, w = join_fast$cycle_base), 2)
  sites_join$circuity_fast_cycle[sites_join$site_name == i] = fast_circuity
  
  mean_busyness = round(weighted.mean(fast_routes$mean_busyness, w = fast_routes$cycle_base), 2)
  sites_join$busyness_fast_cycle[sites_join$site_name == i] = mean_busyness
}

sites_join$in_site_walk_circuity = NA
sites_join$in_site_cycle_circuity = NA
sites_join$in_site_drive_circuity = NA

for(i in sites_join$site_name) {
  f = paste0("data-small/", i, "/in-site-metrics.csv")
  if(file.exists(f)) {
    in_site = sf::read_sf(f)
    
    sites_join$in_site_walk_circuity[sites_join$site_name == i] = in_site$site_walk_circuity
    sites_join$in_site_cycle_circuity[sites_join$site_name == i] = in_site$site_cycle_circuity
    sites_join$in_site_drive_circuity[sites_join$site_name == i] = in_site$site_drive_circuity
  }
}

st_precision(sites_join) = 1000000