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

sites = sf::read_sf("data-small/all-sites.geojson")
sites_join = inner_join(sites,sites_df %>% select(-dwellings_when_complete))

# prototype code to get site metrics
i = sites_join$site_name[1]
sites_join$pconvertable = NA
for(i in sites_join$site_name) {
  f = paste0("data-small/", i, "/desire-lines-many.geojson")
  desire_lines = sf::read_sf(f)
  drive_trips = sum(desire_lines$drive_base)
  drive_dutch = sum(desire_lines$drive_godutch)
  pchanged = round(100 * (drive_trips - drive_dutch) / drive_trips)
  # code to re-add the data to the sites_join table
  sites_join$pconvertable[sites_join$site_name == i] = pchanged
  message(round(100 * (drive_trips - drive_dutch) / drive_trips), " percent in ", i)
}

file.remove("data-small/all-sites.geojson")
file.remove("all-sites.geojson")
sf::write_sf(sites_join,"data-small/all-sites.geojson")
sf::write_sf(sites_join,"all-sites.geojson")
piggyback::pb_upload("all-sites.geojson", tag = "0.1.1")
