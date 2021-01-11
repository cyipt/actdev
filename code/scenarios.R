# Aim: generate scenarios of change associated with new developments

library(tidyverse)

# input data --------------------------------------------------------------

centroids_msoa = pct::get_centroids_ew() 
zones_msoa_national = pct::get_pct(national = TRUE, geography = "msoa", layer = "z")
zones_msoa_national

od = pct::get_od()
# piggyback::pb_download_url("all-sites.geojson")
u = "https://github.com/cyipt/actdev/releases/download/0.1.1/all-sites.geojson"
sites = sf::st_read(u)

# select site of interest
mapview::mapview(sites)
site_number = 16
site = sites[site_number, ]
mapview::mapview(site)
zones_touching_site = zones_msoa_national[site, ]
mapview::mapview(zones_touching_site)

# Generate desire lines ---------------------------------------------------



