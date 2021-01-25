# Aim: generate scenarios of change associated with new developments

library(tidyverse)
library(sf)

# set-up and parameters ---------------------------------------------------

household_size = 2.3 # mean UK household size at 2011 census
max_length = 20000 # maximum length of desire lines in m
site_name = "great-kneighton"   # which site to look at (can change)
min_flow_routes = 5 # threshold above which OD pairs are included
region_buffer_dist = 2000
large_area_buffer = 500

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
  mutate(gradient = 0) #todo: get proper gradients

desire_lines_combined = desire_lines_combined %>% 
  select(geo_code1, geo_code2, all:other, length, gradient, pwalk_commute_base:pdrive_commute_base) %>% 
  rename(all_commute_base = all, walk_commute_base = foot, cycle_commute_base = bicycle, drive_commute_base = car_driver)


# todo: add route data


# Create routes and route networks ----------------------------------------
cl = parallel::makeCluster(parallel::detectCores())

routes_fast = stplanr::route(l = desire_lines_combined, route_fun = cyclestreets::journey, cl = cl)
routes_balanced = stplanr::route(l = desire_lines_combined, route_fun = cyclestreets::journey, cl = cl, plan = "balanced")
routes_quiet = stplanr::route(l = desire_lines_combined, route_fun = cyclestreets::journey, cl = cl, plan = "quietest")

# routes_fast = routes_fast %>% 
#   rename(all_commute_base = all, walk_commute_base = foot, cycle_commute_base = bicycle, drive_commute_base = car_driver)

rnet_fast_walk = stplanr::overline2(routes_fast, "walk_commute_base")
rnet_fast_cycle = stplanr::overline2(routes_fast, "cycle_commute_base")
rnet_fast_drive = stplanr::overline2(routes_fast, "drive_commute_base")

rnet_balanced_walk = stplanr::overline2(routes_balanced, "walk_commute_base")
rnet_balanced_cycle = stplanr::overline2(routes_balanced, "cycle_commute_base")
rnet_balanced_drive = stplanr::overline2(routes_balanced, "drive_commute_base")

rnet_quiet_walk = stplanr::overline2(routes_quiet, "walk_commute_base")
rnet_quiet_cycle = stplanr::overline2(routes_quiet, "cycle_commute_base")
rnet_quiet_drive = stplanr::overline2(routes_quiet, "drive_commute_base")

# Go Dutch scenario -------------------------------------------------------
desire_lines_scenario = desire_lines_combined %>% 
  mutate(pwalk_commute_godutch = case_when(
    length <= 2000 ~ pwalk_commute_base + 0.1, # 10% shift walking
    TRUE ~ pwalk_commute_base)) %>% 
  mutate(pcycle_commute_godutch = pct::uptake_pct_godutch_2020(distance = length, gradient = gradient))

# todo: estimate which proportion of the new walkers/cyclists in the go dutch scenarios would switch from driving, and which proportion would switch from other modes
desire_lines_scenario = desire_lines_scenario %>% 
  mutate(walk_commute_godutch = all_commute_base * pwalk_commute_godutch) %>% 
  mutate(cycle_commute_godutch = all_commute_base * pcycle_commute_godutch) %>% 
  mutate(drive_commute_godutch = case_when(
    drive_commute_base + (cycle_commute_base - cycle_commute_godutch) + (walk_commute_base - walk_commute_godutch) >= 0 ~ 
      drive_commute_base + (cycle_commute_base - cycle_commute_godutch) + (walk_commute_base - walk_commute_godutch),
    TRUE ~ 0)
  ) %>% 
  mutate(pdrive_commute_godutch = drive_commute_godutch / all_commute_base) %>%
  select(geo_code1:pdrive_commute_base, walk_commute_godutch:drive_commute_godutch, pwalk_commute_godutch, pcycle_commute_godutch, pdrive_commute_godutch)

dsn = file.path("data-small", site_name, "all-census-od.csv")
readr::write_csv(desire_lines_scenario, file = dsn)


# Mode split summary by distance ------------------------------------------

# The Go Dutch scenario does not work for routes with a distance > 30km
# mapview(desire_lines_scenario %>% filter(length >+ 30000))

sum_total = sum(desire_lines_scenario$all_commute_base)

mode_split = desire_lines_scenario %>% 
  st_drop_geometry() %>%
  mutate(desire_lines_scenario, length = case_when(geo_code1 == geo_code2 ~ 100, TRUE ~ length)) %>% # doesn't change it
  select(length, all_commute_base, pwalk_commute_base:pdrive_commute_base, pwalk_commute_godutch:pdrive_commute_godutch) %>% 
  mutate(length_cat = case_when(
    length < 1000 ~ "a.0-1",
    length < 3000 ~ "b.1-3",
    length < 6000 ~ "c.3-6",
    length < 10000 ~ "d.6-10",
    length < 15000 ~ "e.10-15",
    length < 20000 ~ "f.15-20",
    length < 30000 ~ "g.20-30",
    length >= 30000 ~ "h.30+",
  )) %>% 
  group_by(length_cat) %>% 
  summarise(pwalk_commute_base = weighted.mean(pwalk_commute_base, w = all_commute_base),
            pcycle_commute_base = weighted.mean(pcycle_commute_base, w = all_commute_base),
            pdrive_commute_base = weighted.mean(pdrive_commute_base, w = all_commute_base),
            pwalk_commute_godutch = weighted.mean(pwalk_commute_godutch, w = all_commute_base),
            pcycle_commute_godutch = weighted.mean(pcycle_commute_godutch, w = all_commute_base),
            pdrive_commute_godutch = weighted.mean(pdrive_commute_godutch, w = all_commute_base),
            total = sum(all_commute_base)/ sum_total
            ) %>% 
  mutate(across(where(is.numeric), round, 2),
         pwalk_commute_godutch = ifelse(
           length_cat == "h.30+", NA, pwalk_commute_godutch),
         pcycle_commute_godutch = ifelse(
           length_cat == "h.30+", NA, pcycle_commute_godutch),
         pdrive_commute_godutch = ifelse(
           length_cat == "h.30+", NA, pdrive_commute_godutch))

dsn = file.path("data-small", site_name, "mode-split.csv")
readr::write_csv(mode_split, file = dsn)

# Round decimals and select sets of desire lines --------------------------
# desire_lines_rounded = desire_lines_scenario %>% 
#   mutate(across(where(is.numeric), round, 6))

desire_lines_rounded = desire_lines_scenario %>%
  mutate(across(c(all_commute_base:other, walk_commute_godutch:drive_commute_godutch), smart.round)) #%>% 
  # rename(all_commute_base = all, walk_commute_base = foot, cycle_commute_base = bicycle, drive_commute_base = car_driver)

st_precision(desire_lines_rounded) = 1000000

desire_lines_20km = desire_lines_rounded %>% 
  filter(length <= max_length)

desire_lines_threshold = desire_lines_rounded %>%
  filter(all_commute_base >= min_flow_routes)

desire_lines_bounding = desire_lines_20km %>% 
  filter(geo_code2 %in% desire_lines_threshold$geo_code2)

# Large study area MSOAs --------------------------------------------------
# work_zone = inner_join(zones_msoa_national %>% select(geo_code), 
#                        desire_lines_bounding %>% st_drop_geometry() %>% select(geo_code2),
#                        by = c("geo_code" = "geo_code2"))
# home_zone = inner_join(zones_msoa_national %>% select(geo_code), 
#                        desire_lines_bounding %>% st_drop_geometry() %>% select(geo_code1),
#                        by = c("geo_code" = "geo_code1"))
# home_zone = unique(home_zone)
# zones_touching_large_study_area = bind_rows(home_zone, work_zone) %>%
#   unique()

large_study_area = sf::st_convex_hull(sf::st_union(desire_lines_bounding))
large_study_area = stplanr::geo_buffer(large_study_area, dist = large_area_buffer)
# mapview(desire_lines_bounding) + mapview(large_study_area)


# zones_touching_large_study_area2 = zones_msoa_national[convex_hull, , op = sf::st_intersects]

# large_study_area = st_union(zones_touching_large_study_area)
# large_study_area = sfheaders::sf_remove_holes(large_study_area)
# large_study_area2 = nngeo::st_remove_holes(large_study_area)
# mapview(desire_lines_bounding) + mapview(large_study_area)

# zones_without_holes = zones_msoa_national[large_study_area, , op = sf::st_within]
# zones_without_holes = zones_without_holes %>% 
#   select(geo_code)
# st_precision(zones_without_holes) = 1000000

dsn = file.path("data-small", site_name, "large-study-area.geojson")
sf::write_sf(large_study_area, dsn = dsn)
# sf::write_sf(zones_without_holes, dsn = dsn)

desire_lines_many = desire_lines_rounded[large_study_area, , op = sf::st_within]
desire_lines_many = desire_lines_many %>% 
  select(geo_code1, geo_code2, all_commute_base, walk_commute_base, cycle_commute_base, drive_commute_base, walk_commute_godutch:drive_commute_godutch)

dsn = file.path("data-small", site_name, "desire-lines-many.geojson")
sf::write_sf(desire_lines_many, dsn = dsn)

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

lsoa_c = st_centroid(jts0501)
lsoa_c = lsoa_c[large_study_area, ,op = sf::st_within]
lsoas_inside = filter(jts0501, LSOA_code %in% lsoa_c$LSOA_code)

lsoas_bounding = jts0501[desire_lines_bounding, ,op = sf::st_intersects]
lsoas_both = bind_rows(lsoas_inside, lsoas_bounding) %>% 
  unique()

lsoa_study_area = st_union(lsoas_both)
lsoa_study_area = sfheaders::sf_remove_holes(lsoa_study_area)
# lsoa_study_area2 = nngeo::st_remove_holes(lsoa_study_area)


lsoas_all = jts0501[lsoa_study_area, , op = sf::st_within]

# mapview(lsoas_all) + mapview(desire_lines_bounding)

lsoas_all$weightedJobsPTt = apply(
  X = st_drop_geometry(lsoas_all[c("X100EmpPTt", "X500EmpPTt", "X5000EmpPTt")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

lsoas_all$weightedJobsCyct = apply(
  X = st_drop_geometry(lsoas_all[c("X100EmpCyct", "X500EmpCyct", "X5000EmpCyct")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

lsoas_all$weightedJobsCart = apply(
  X = st_drop_geometry(lsoas_all[c("X100EmpCart", "X500EmpCart", "X5000EmpCart")]),
  MARGIN = 1,
  FUN = weighted.mean,
  w = c(100, 500, 5000)
)

lsoas_all = lsoas_all %>% 
  select(LSOA_code, weightedJobsPTt, weightedJobsCyct, weightedJobsCart)

j2 = jts0502 %>% 
  select(LSOA_code, PSPTt, PSCyct, PSCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j2)

j3 = jts0503 %>% 
  select(LSOA_code, SSPTt, SSCyct, SSCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j3)

j4 = jts0504 %>% 
  select(LSOA_code, FEPTt, FECyct, FECart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j4)

j5 = jts0505 %>% 
  select(LSOA_code, GPPTt, GPCyct, GPCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j5)

j6 = jts0506 %>% 
  select(LSOA_code, HospPTt, HospCyct, HospCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j6)

j7 = jts0507 %>% 
  select(LSOA_code, FoodPTt, FoodCyct, FoodCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j7)

j8 = jts0508 %>% 
  select(LSOA_code, TownPTt, TownCyct, TownCart) %>% 
  st_drop_geometry()
lsoas_all = inner_join(lsoas_all, j8)

st_precision(lsoas_all) = 1000000

dsn = file.path("data-small", site_name, "jts-lsoas.geojson")
write_sf(lsoas_all, dsn = dsn)

