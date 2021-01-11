# Aim: generate scenarios of change associated with new developments

library(tidyverse)
library(tmap)
tmap_mode("view")

# set-up and parameters ---------------------------------------------------

max_length = 20000 # maximum length of desire lines in m
site_number = 16   # which site to look at (can change)

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
site = sites[site_number, ]
mapview::mapview(site)
zones_touching_site = zones_msoa_national[site, , op = sf::st_intersects]
mapview::mapview(zones_touching_site)

# Generate desire lines ---------------------------------------------------

# todo: route to site - Joey has done can add code here

od_site = od %>% 
  filter(geo_code1 %in% zones_touching_site$geo_code) %>% 
  filter(geo_code2 %in% centroids_msoa$msoa11cd) %>% 
  filter(geo_code1 != geo_code2) # note: not accounting for intrazonal flows
# intra-zonal flows could added later

desire_lines_site = od::od_to_sf(x = od_site, z = centroids_msoa)
mapview::mapview(desire_lines_site)
desire_lines_site$length = stplanr::geo_length(desire_lines_site)
desire_lines_site = desire_lines_site %>% 
  filter(length <= max_length) %>% 
  mutate(pwalk_commute_base = foot/all) %>% 
  mutate(pcycle_commute_base = bicycle/all) %>% 
  mutate(pdrive_commute_base = car_driver/all) %>% 
  mutate(gradient = 0)

# todo: add PT

# mapview::mapview(desire_lines_site)
co = c("pwalk_commute_base", "pcycle_commute_base", "pdrive_commute_base")
b = tmaptools::bb(desire_lines_site, ext = 0.2)
tm_shape(desire_lines_site, bbox = b) +
  tm_lines(lwd = "all", scale = 9, col = co, palette = "viridis")

# todo: add empirical data on 'new homes' effect
# todo: add route data
desire_lines_site = desire_lines_site %>% 
  mutate(pcycle_commute_godutch = pct::uptake_pct_godutch_2020(distance = length, gradient = gradient)) %>% 
  mutate(pwalk_commute_godutch = case_when(
    length <= 2000 ~ pwalk_commute_base + 0.1, # 10% shift walking
    TRUE ~ pwalk_commute_base 
    )
    )  
plot(desire_lines_site$pwalk_commute_godutch, desire_lines_site$pwalk_commute_base, cex = desire_lines_site$all / 100)
plot(desire_lines_site$length, desire_lines_site$pcycle_commute_godutch, cex = desire_lines_site$all / 100)
points(desire_lines_site$length, desire_lines_site$pwalk_commute_godutch, cex = desire_lines_site$all / 100, col = "blue")


desire_lines_scenario = desire_lines_site %>% 
  mutate(bicycle_commute_godutch = all * pcycle_commute_godutch) %>% 
  mutate(walk_commute_godutch = all * pwalk_commute_godutch) %>% 
  mutate(car_commute_godutch = car_driver + (bicycle - bicycle_commute_godutch) +
           (foot - walk_commute_godutch)) %>% 
  mutate(pcar_commute_godutch = car_commute_godutch / all)

co_dutch = c("walk_commute_godutch", "bicycle_commute_godutch", "car_commute_godutch")
co_pdutch = c("pwalk_commute_godutch", "pcycle_commute_godutch", "pcar_commute_godutch")

tm_shape(desire_lines_scenario, bbox = b) +
  tm_lines(lwd = "all", scale = 9, col = co_pdutch, palette = "viridis")

tm_shape(desire_lines_scenario, bbox = b) +
  tm_lines(lwd = "all", scale = 9, col = co_dutch, palette = "viridis")


