# Aim: make table of sites

library(tidyverse)

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

# prototype code to get site metrics
i = sites_join$site_name[1]
sites_join$pactive = NA
sites_join$pconvertable = NA
sites_join$pconvertable_near = NA
sites_join$pfutureactive = NA
sites_join$pfutureactive_optimistic = NA
for(i in sites_join$site_name) {
  f = paste0("data-small/", i, "/desire-lines-many.geojson")
  desire_lines = sf::read_sf(f) 
  desire_lines = desire_lines %>% filter(purpose == "commute") # take out journeys to towns
  f = paste0("data-small/", i, "/all-census-od.csv")
  all_desire_lines = sf::read_sf(f)
  
  all_desire_lines = all_desire_lines %>% mutate(across(all:pdrive_base, as.numeric))
  prop_near = sum(desire_lines$all_base) / sum(all_desire_lines$all) #proportion of commutes that are represented in desire_lines_many 
  all_trips = sum(all_desire_lines$all)
  drive_trips = sum(all_desire_lines$car_driver)
  active_base = sum(all_desire_lines$foot) + sum(all_desire_lines$bicycle)
  
  drive_near = sum(desire_lines$drive_base)
  drive_dutch = sum(desire_lines$drive_godutch)
  active_near = sum(desire_lines$walk_base) + sum(desire_lines$cycle_base)
  active_dutch = sum(desire_lines$walk_godutch) + sum(desire_lines$cycle_godutch)
  
  pchanged = round(100 * (drive_near - drive_dutch) / drive_trips)  
  pchanged_ofnear = round(100 * (drive_near - drive_dutch) / drive_near)
  pactive = round(100 * active_base / all_trips)
  pfutureactive = round(100 * (active_base + active_dutch - active_near) / all_trips)
  # to correct for missing desire lines in small sites: calculate the % by which active travel has increased in the mapped desire lines, then assume it increases by the same proportion in unmapped desire lines (yes this is overly optimistic because the unmapped ones will be longer, but at least it evens things out between sites) 
  pactive_increase = active_dutch / active_near
  pfutureactive_optimistic = round(100 * (active_base / all_trips * pactive_increase))
  
  # code to re-add the data to the sites_join table
  sites_join$pactive[sites_join$site_name == i] = pactive
  sites_join$pconvertable[sites_join$site_name == i] = pchanged
  sites_join$pconvertable_near[sites_join$site_name == i] = pchanged_ofnear
  sites_join$pfutureactive[sites_join$site_name == i] = pfutureactive
  sites_join$pfutureactive_optimistic[sites_join$site_name == i] = pfutureactive_optimistic
  # message(round(100 * (drive_trips - drive_dutch) / drive_trips), " percent in ", i)
}


file.remove("data-small/all-sites.geojson")
file.remove("all-sites.geojson")
sf::write_sf(sites_join,"data-small/all-sites.geojson")
sf::write_sf(sites_join,"all-sites.geojson")
piggyback::pb_upload("all-sites.geojson", tag = "0.1.1")
