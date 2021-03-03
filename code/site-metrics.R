# Aim: make table of sites

library(tidyverse)
library(spatstat)
library(sf)

# sites = sf::read_sf("data-small/all-sites.geojson")
# sites_df = sf::st_drop_geometry(sites)
# 
# sites_df
# 
# sites_df$full_name = sites_df$site_name
# write_csv(sites_df, "data-small/all-sites.csv")

# add columns to the csv
# sites_df = read_csv("data-small/all-sites.csv")
# sites_df$planning_url = NA
# write_csv(sites_df, "data-small/all-sites.csv")
# 
# sites = sf::read_sf("data-small/all-sites.geojson")
# sites_join = inner_join(sites,sites_df %>% select(-dwellings_when_complete))
sites_join = sf::read_sf("data-small/all-sites.geojson")

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
  desire_town = desire_lines %>% filter(purpose == "town")
  desire_lines = desire_lines %>% filter(purpose == "commute") # take out journeys to towns
  f = paste0("data-small/", i, "/all-census-od.csv")
  all_desire_lines = read_csv(f)
  site_boundary = sites_join$geometry[sites_join$site_name == i]
  site_line = st_cast(site_boundary,"LINESTRING")
  f = paste0("data-small/", i, "/routes-fast.geojson")
  fast_routes = sf::read_sf(f)
  
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
  
  sites_join$distance_to_town[sites_join$site_name == i] = round(desire_town$length / 1000, 1)
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

# add in circuity measures

file.remove("data-small/all-sites.geojson")
file.remove("all-sites.geojson")
sf::write_sf(sites_join,"data-small/all-sites.geojson")
sf::write_sf(sites_join,"all-sites.geojson")
piggyback::pb_upload("all-sites.geojson", tag = "0.1.1")
