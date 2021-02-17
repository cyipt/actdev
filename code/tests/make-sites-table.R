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
sites_df = read_csv("data-small/all-sites.csv")
sites_df$planning_url = NA
write_csv(sites_df, "data-small/all-sites.csv")

# prototype code to get site metrics
i = sites_df$site_name[1]
for(i in sites_df$site_name) {
  f = paste0("data-small/", i, "/desire-lines-many.geojson")
  desire_lines = sf::read_sf(f)
  all_trips = sum(desire_lines$all_base)
  dutch_cycling = sum(desire_lines$cycle_godutch)
  # code to re-add the data to the sites_df table
  message(round(100 * dutch_cycling / all_trips), " percent in ", i)
}
