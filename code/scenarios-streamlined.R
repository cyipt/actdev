# Aim: generate scenarios of change associated with new developments

library(tidyverse)
library(sf)

# set-up and parameters ---------------------------------------------------

household_size = 2.3 # mean UK household size at 2011 census
max_length = 20000 # maximum length of desire lines in m
site_name = "chapelford"   # which site to look at (can change)
# min_flow_routes = 5 # threshold above which OD pairs are included
region_buffer_dist = 2000

smart.round = function(x) {
  y = floor(x)
  indices = utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y
}

# generic input data --------------------------------------------------------------
centroids_msoa = pct::get_centroids_ew() 
centroids_msoa = sf::st_transform(centroids_msoa, 4326)
zones_msoa_national = pct::get_pct(national = TRUE, geography = "msoa", layer = "z")
st_precision(zones_msoa_national) = 1000000

od = pct::get_od()
u = "https://github.com/cyipt/actdev/releases/download/0.1.1/all-sites.geojson"
sites = sf::st_read(u)
st_precision(sites) = 1000000

# 2011 MSOA populations
msoa_pops = readxl::read_xls(path = "data/mid2011msoaunformattedfile.xls", sheet = "Mid-2011 Persons", )
msoa_pops = msoa_pops %>% 
  select(geo_code1 = Code, msoa_population = "All Ages")

# estimated site populations
site_pops = sites %>% 
  st_drop_geometry() %>% 
  mutate(site_population = dwellings_when_complete * household_size)

# jts data - SLOW
all_jts_tables = paste0("jts050", 1:8)
i = all_jts_tables[1]
for(i in all_jts_tables){
  year = 2017
  f = paste0(i, "-", year, ".geojson")
  # piggyback::pb_download(f, tag = "0.1.2")
  f2 = sf::read_sf(f)
  assign(i, f2)
  rm(f2)
}

# Select site of interest -------------------------------------------------
site = sites[sites$site_name == site_name, ]

path = file.path("data-small", site_name)
dir.create(path = path)

zones_touching_site = zones_msoa_national[site, , op = sf::st_intersects]

zones_touching_site$overlap_size = units::drop_units(st_area(zones_touching_site))
zones_touching_site = zones_touching_site %>% 
  filter(overlap_size > 10000) %>% 
  select(-overlap_size)


# Route from site centroid (rather than MSOA centroid) --------------------
# `disaggregate.R` changes this to route from a random selection of homes within the site, to better represent the accessibility of the site as a whole
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
  filter(geo_code2 %in% centroids_msoa$msoa11cd)
# intra-zonal flows are included, with the desire line going from the site centroid to the msoa centroid
# intra-zonal flows could later be represented using more detailed od-workplace zone data.

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

dsn = file.path("data-small", site_name, "all-census-od.csv")
readr::write_csv(desire_lines_scenario, file = dsn)

# Round decimals and select sets of desire lines --------------------------
# desire_lines_rounded = desire_lines_scenario %>% 
#   mutate(across(where(is.numeric), round, 6))

desire_lines_rounded = desire_lines_scenario %>%
  mutate(across(c(all:other, walk_commute_godutch:drive_commute_godutch), smart.round)) %>% 
  rename(all_commute_base = all, walk_commute_base = foot, cycle_commute_base = bicycle, drive_commute_base = car_driver)

st_precision(desire_lines_rounded) = 1000000

desire_lines_20km = desire_lines_rounded %>% 
  filter(length <= max_length)
desire_lines_20km = desire_lines_20km %>% 
  select(geo_code1, geo_code2, all_commute_base, walk_commute_base, cycle_commute_base, drive_commute_base, walk_commute_godutch:drive_commute_godutch)

dsn = file.path("data-small", site_name, "desire-lines-many.geojson")
sf::write_sf(desire_lines_20km, dsn = dsn)

# desire_lines_5 = desire_lines_rounded %>% 
#   filter(all >= min_flow_routes)

# Large study area MSOAs --------------------------------------------------
work_zone = inner_join(zones_msoa_national %>% select(geo_code), 
                       desire_lines_20km %>% st_drop_geometry() %>% select(geo_code2),
                       by = c("geo_code" = "geo_code2"))
home_zone = inner_join(zones_msoa_national %>% select(geo_code), 
                       desire_lines_20km %>% st_drop_geometry() %>% select(geo_code1),
                       by = c("geo_code" = "geo_code1"))
home_zone = unique(home_zone)
zones_touching_large_study_area = bind_rows(home_zone, work_zone) %>%
  unique()

# convex_hull = sf::st_convex_hull(sf::st_union(desire_lines_20km))
# zones_touching_large_study_area2 = zones_msoa_national[convex_hull, , op = sf::st_intersects]

large_study_area = st_union(zones_touching_large_study_area)
large_study_area1 = sfheaders::sf_remove_holes(large_study_area)
large_study_area2 = nngeo::st_remove_holes(large_study_area)

zones_without_holes = zones_msoa_national[large_study_area, , op = sf::st_within]
zones_without_holes = zones_without_holes %>% 
  select(geo_code)
st_precision(zones_without_holes) = 1000000

dsn = file.path("data-small", site_name, "large-study-area-zones.geojson")
sf::write_sf(zones_without_holes, dsn = dsn)

# Get region of interest from desire lines --------------------------------
min_flow_map = site_population / 80
desire_lines_busy = desire_lines_rounded %>% 
  filter(all_commute_base >= min_flow_map)

convex_hull = sf::st_convex_hull(sf::st_union(desire_lines_busy))
study_area = stplanr::geo_buffer(convex_hull, dist = region_buffer_dist)
st_precision(study_area) = 1000000

# zones_touching_small_study_area = zones_msoa_national[study_area, , op = sf::st_intersects]

dsn = file.path("data-small", site_name, "small-study-area.geojson")
sf::write_sf(study_area, dsn = dsn)

# Desire lines for small study area ---------------------------------------
desire_lines_few = desire_lines_rounded[study_area, , op = sf::st_within]
desire_lines_few = desire_lines_few %>% 
  select(geo_code1, geo_code2, all_commute_base, walk_commute_base, cycle_commute_base, drive_commute_base, walk_commute_godutch:drive_commute_godutch)

dsn = file.path("data-small", site_name, "desire-lines-few.geojson")
sf::write_sf(desire_lines_few, dsn = dsn)


# Get LSOA level JTS data for site ----------------------------------------
# employ = jts::get_jts_data(table = "jts0501", output_format = "sf")
# employ_site = employ[site, , op = sf::st_intersects]
# employ_site = jts0501[site, , op = sf::st_intersects]

employ_site = st_intersection(jts0501, site)
employ_site$overlap_size = units::drop_units(st_area(employ_site))
access_employ = employ_site %>% 
  filter(overlap_size > 10000)
# mapview(access_employ)
# names(access_employ) = sub("X","", names(access_employ))

access_employ$weightedJobsPTt = apply(
  X = st_drop_geometry(access_employ[c("X100EmpPTt", "X500EmpPTt", "X5000EmpPTt")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_employ$weightedJobsCyct = apply(
  X = st_drop_geometry(access_employ[c("X100EmpCyct", "X500EmpCyct", "X5000EmpCyct")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_employ$weightedJobsCart = apply(
  X = st_drop_geometry(access_employ[c("X100EmpCart", "X500EmpCart", "X5000EmpCart")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_site = access_employ %>% 
  select(LSOA_code, weightedJobsPTt, weightedJobsCyct, weightedJobsCart)

j2 = jts0502 %>% 
  select(LSOA_code, PSPTt, PSCyct, PSCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j2)

j3 = jts0503 %>% 
  select(LSOA_code, SSPTt, SSCyct, SSCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j3)

j4 = jts0504 %>% 
  select(LSOA_code, FEPTt, FECyct, FECart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j4)

j5 = jts0505 %>% 
  select(LSOA_code, GPPTt, GPCyct, GPCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j5)

j6 = jts0506 %>% 
  select(LSOA_code, HospPTt, HospCyct, HospCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j6)

j7 = jts0507 %>% 
  select(LSOA_code, FoodPTt, FoodCyct, FoodCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j7)

j8 = jts0508 %>% 
  select(LSOA_code, TownPTt, TownCyct, TownCart) %>% 
  st_drop_geometry()
access_site = inner_join(access_site, j8)

access_means = access_site %>% 
  mutate(site_name = site_name) %>% 
  group_by(site_name) %>% 
  summarise(across(c(weightedJobsPTt:weightedJobsCart, PSPTt:TownCart), mean))

site_data = inner_join(site, st_drop_geometry(access_means), by = "site_name")
st_precision(site_data) = 1000000

dsn = file.path("data-small", site_name, "site.geojson")
write_sf(site_data, dsn = dsn)


# JTS data for surrounding LSOAs ------------------------------------------
# large_study_area2 = st_set_crs(large_study_area2, 4326)
# large_study_area2 = st_make_valid(large_study_area2)

employ_lsoas2 = st_intersection(jts0501, large_study_area2) # doesn't work in chapelford, works in great kneighton
employ_lsoas$overlap_size = units::drop_units(st_area(employ_lsoas))
access_lsoas_employ = employ_lsoas %>% 
  filter(overlap_size > 10000)
# mapview(access_lsoas_employ)
# names(access_lsoas_employ) = sub("X","", names(access_lsoas_employ))

access_lsoas_employ$weightedJobsPTt = apply(
  X = st_drop_geometry(access_lsoas_employ[c("X100EmpPTt", "X500EmpPTt", "X5000EmpPTt")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_lsoas_employ$weightedJobsCyct = apply(
  X = st_drop_geometry(access_lsoas_employ[c("X100EmpCyct", "X500EmpCyct", "X5000EmpCyct")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_lsoas_employ$weightedJobsCart = apply(
  X = st_drop_geometry(access_lsoas_employ[c("X100EmpCart", "X500EmpCart", "X5000EmpCart")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

access_lsoas = access_lsoas_employ %>% 
  select(LSOA_code, weightedJobsPTt, weightedJobsCyct, weightedJobsCart)

j2 = jts0502 %>% 
  select(LSOA_code, PSPTt, PSCyct, PSCart) %>% 
  st_drop_geometry()
access_lsoas = inner_join(access_lsoas, j2)

j3 = jts0503 %>% 
  select(LSOA_code, SSPTt, SSCyct, SSCart) %>% 
  st_drop_geometry()
access_lsoas = inner_join(access_lsoas, j3)

j4 = jts0504 %>% 
  select(LSOA_code, FEPTt, FECyct, FECart) %>% 
  st_drop_geometry()
access_lsoas = inner_join(access_lsoas, j4)

j5 = jts0505 %>% 
  select(LSOA_code, GPPTt, GPCyct, GPCart) %>% 
  st_drop_geometry()
access_lsoas = inner_join(access_lsoas, j5)

j6 = jts0506 %>% 
  select(LSOA_code, HospPTt, HospCyct, HospCart) %>% 
  st_drop_geometry()
access_lsoas = inner_join(access_lsoas, j6)

j7 = jts0507 %>% 
  select(LSOA_code, FoodPTt, FoodCyct, FoodCart) %>% 
  st_drop_geometry()
access_lsoas = inner_join(access_lsoas, j7)

j8 = jts0508 %>% 
  select(LSOA_code, TownPTt, TownCyct, TownCart) %>% 
  st_drop_geometry()
access_lsoas = inner_join(access_lsoas, j8)

st_precision(access_lsoas) = 1000000

dsn = file.path("data-small", site_name, "jts-lsoas.geojson")
write_sf(access_lsoas, dsn = dsn)

