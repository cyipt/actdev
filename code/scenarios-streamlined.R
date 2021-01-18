# Aim: generate scenarios of change associated with new developments

library(tidyverse)
library(sf)

# set-up and parameters ---------------------------------------------------

household_size = 2.3 # mean UK household size at 2011 census
max_length = 20000 # maximum length of desire lines in m
site_name = "chapelford"   # which site to look at (can change)
# min_flow_routes = 5 # threshold above which OD pairs are included
region_buffer_dist = 2000

# generic input data --------------------------------------------------------------
centroids_msoa = pct::get_centroids_ew() 
centroids_msoa = sf::st_transform(centroids_msoa, 4326)
zones_msoa_national = pct::get_pct(national = TRUE, geography = "msoa", layer = "z")

od = pct::get_od()
u = "https://github.com/cyipt/actdev/releases/download/0.1.1/all-sites.geojson"
sites = sf::st_read(u)

# 2011 MSOA populations
msoa_pops = readxl::read_xls(path = "data/mid2011msoaunformattedfile.xls", sheet = "Mid-2011 Persons", )
msoa_pops = msoa_pops %>% 
  select(geo_code1 = Code, msoa_population = "All Ages")

# estimated site populations
site_dwellings = read_csv("data/site-populations.csv")
site_pops = site_dwellings %>% 
  mutate(site_population = dwellings_when_complete * household_size)

# Select site of interest -------------------------------------------------
site = sites[sites$site_name == site_name, ]

path = file.path("data-small", site_name)
dir.create(path = path)

dsn = file.path("data-small", site_name, "site-boundary.geojson")
sf::write_sf(obj = site, dsn = dsn)

zones_touching_site = zones_msoa_national[site, , op = sf::st_intersects]


# Route from site centroid (rather than MSOA centroid) --------------------
# this could be changed to route from a random selection of homes within the site, to better represent the accessibility of the site as a whole
site_centroid = site %>% 
  st_transform(27700) %>% 
  st_centroid() %>% 
  st_transform(4326)

zone_data = zones_touching_site %>% 
  st_drop_geometry() %>%
  mutate(site_name = site$site_name)
site_c = right_join(site_centroid, zone_data) %>%
  select(geo_code, site_name)

# Generate desire lines ---------------------------------------------------
od_site = od %>% 
  filter(geo_code1 %in% zones_touching_site$geo_code) %>% 
  filter(geo_code2 %in% centroids_msoa$msoa11cd) %>% 
  filter(geo_code1 != geo_code2) # note: not accounting for intrazonal flows. But where the site lies within 2 or more MSOAs, flows from one to the other of these will still be included.
# intra-zonal flows could be added later, using more detailed od-workplace zone data. Or we could simply route from the site centroid to the MSOA centroid.

desire_lines_site = od::od_to_sf(x = od_site, z = site_c, zd = centroids_msoa)
desire_lines_site = desire_lines_site %>% 
  mutate(site_name = site_name)


# Adjust flows to represent site population, not MSOA population(s) -------
# for both MSOAs and development sites, these are entire populations, not commuter populations
desire_lines_site = inner_join(desire_lines_site, msoa_pops)
desire_lines_site = inner_join(desire_lines_site, site_pops)
site_population = unique(desire_lines_site$site_population)
unique_msoa_pops = desire_lines_site %>% 
  st_drop_geometry() %>% 
  select(geo_code1, msoa_population) %>%
  unique()
sum_msoa_pops = sum(unique_msoa_pops$msoa_population)
desire_lines_site = desire_lines_site %>% 
  mutate(sum_msoa_pops = sum_msoa_pops)

# keeping converted flows in the original columns
desire_lines_pops = desire_lines_site %>% 
  mutate(across(all:other, .fns = ~ ./ sum_msoa_pops * site_population))

# todo: add empirical data on 'new homes' effect (residents of new homes are more likely to drive than residents of older homes)
# could also adjust the base walking and cycling mode shares in response to the difference between journey distance from the site centroid as compared to journey distance from the MSOA centroid (eg in Cambridge, the MSOA centroid is a fair bit closer to the city centre than the site centroid, which could explain why such a high proportion of commuters are shown walking to work in the city centre)  

# For sites with 2 or more origin MSOAs, combine flows to avoid having multiple desire lines to the same destination MSOA
desire_lines_combined = desire_lines_pops %>% 
  group_by(geo_code2) %>% 
  summarise(
    geo_code1 = geo_code1[1], # do we even need this?
    across(all:other, sum)
  )

desire_lines_combined$length = stplanr::geo_length(desire_lines_combined)

# todo: add PT
desire_lines_combined = desire_lines_combined %>% 
  mutate(pwalk_commute_base = foot/all) %>% 
  mutate(pcycle_commute_base = bicycle/all) %>% 
  mutate(pdrive_commute_base = car_driver/all) %>% 
  mutate(gradient = 0)

desire_lines_combined = desire_lines_combined %>% 
  select(geo_code1, geo_code2, all:other, length, gradient, pwalk_commute_base:pdrive_commute_base)

# todo: add route data

# Go Dutch scenario -------------------------------------------------------
desire_lines_scenario = desire_lines_combined %>% 
  mutate(pwalk_commute_godutch = case_when(
    length <= 2000 ~ pwalk_commute_base + 0.1, # 10% shift walking
    TRUE ~ pwalk_commute_base)) %>% 
  mutate(pcycle_commute_godutch = pct::uptake_pct_godutch_2020(distance = length, gradient = gradient))

# todo: estimate which proportion of the new walkers/cyclists in the go dutch scenarios would switch from driving, and which proportion would switch from other modes
desire_lines_scenario = desire_lines_scenario %>% 
  mutate(walk_commute_godutch = all * pwalk_commute_godutch) %>% 
  mutate(cycle_commute_godutch = all * pcycle_commute_godutch) %>% 
  mutate(drive_commute_godutch = case_when(
    car_driver + (bicycle - cycle_commute_godutch) + (foot - walk_commute_godutch) >= 0 ~ 
      car_driver + (bicycle - cycle_commute_godutch) + (foot - walk_commute_godutch),
    TRUE ~ 0)
  ) %>% 
  mutate(pdrive_commute_godutch = drive_commute_godutch / all) %>%
  select(geo_code1:pdrive_commute_base, walk_commute_godutch:drive_commute_godutch, pwalk_commute_godutch, pcycle_commute_godutch, pdrive_commute_godutch)

#######

desire_lines_rounded = desire_lines_scenario %>% 
  mutate(across(where(is.numeric), round, 6))
st_precision(desire_lines_rounded) = 1000000

dsn = file.path("data-small", site_name, "all-census-od.csv")
readr::write_csv(desire_lines_rounded, file = dsn)

desire_lines_20km = desire_lines_rounded %>% 
  filter(length <= max_length)
desire_lines_20km = desire_lines_20km %>% 
  select(geo_code1, geo_code2, all_commute_base = all, cycle_commute_base = bicycle, walk_commute_base = foot, drive_commute_base = car_driver, walk_commute_godutch:drive_commute_godutch)
dsn = file.path("data-small", site_name, "desire-lines-many.geojson")
sf::write_sf(desire_lines_20km, dsn = dsn)

# desire_lines_5 = desire_lines_rounded %>% 
#   filter(all >= min_flow_routes)

# Get region of interest from desire lines --------------------------------
min_flow_map = site_population / 80
desire_lines_busy = desire_lines_rounded %>% 
  filter(all >= min_flow_map)

convex_hull = sf::st_convex_hull(sf::st_union(desire_lines_busy))
study_area = stplanr::geo_buffer(convex_hull, dist = region_buffer_dist)


zones_touching_study_area = zones_msoa_national[study_area, , op = sf::st_intersects]

dsn = file.path("data-small", site_name, "small-study-area.geojson")
sf::write_sf(study_area, dsn = dsn)

# Add scenarios of change -------------------------------------------------

desire_lines_few = desire_lines_rounded[study_area, , op = sf::st_within]
desire_lines_few = desire_lines_few %>% 
  select(geo_code1, geo_code2, all_commute_base = all, cycle_commute_base = bicycle, walk_commute_base = foot, drive_commute_base = car_driver, walk_commute_godutch:drive_commute_godutch)

dsn = file.path("data-small", site_name, "desire-lines-few.geojson")
sf::write_sf(desire_lines_few, dsn = dsn)

