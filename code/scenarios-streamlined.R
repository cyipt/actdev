# Aim: generate scenarios of change associated with new developments

library(tidyverse)
library(sf)
library(stplanr)

# set-up and parameters ---------------------------------------------------

setwd("~/cyipt/actdev")

household_size = 2.3 # mean UK household size at 2011 census
max_length = 20000 # maximum length of desire lines in m
site_name = "great-kneighton"   # which site to look at (can change)
min_flow_routes = 10 # threshold above which OD pairs are included
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
sf::st_crs(zones_msoa_national)
st_precision(zones_msoa_national) = 1000000

od = pct::get_od()
u = "https://github.com/cyipt/actdev/releases/download/0.1.1/all-sites.geojson"
sites = sf::st_read(u)
st_precision(sites) = 1000000

# 2011 MSOA populations
u2 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fmiddlesuperoutputareamidyearpopulationestimates%2fmid2011/mid2011msoaunformattedfile.xls"
f = "data/mid2011msoaunformattedfile.xls"
if(!file.exists(f)) {
  download.file(u2, f)
}
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
  if(! file.exists(f)) piggyback::pb_download(f, tag = "0.1.2")
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
  mutate(pdrive_commute_base = car_driver/all)

desire_lines_combined = desire_lines_combined %>% 
  select(geo_code1, geo_code2, all:other, length, pwalk_commute_base:pdrive_commute_base) %>%
  mutate(across(where(is.numeric), round, 6))

st_precision(desire_lines_combined) = 1000000

dsn = file.path("data-small", site_name, "all-census-od.csv")
readr::write_csv(desire_lines_combined, file = dsn)

# Round decimals and select sets of desire lines --------------------------
desire_lines_rounded = desire_lines_combined %>% 
  rename(all_commute_base = all, walk_commute_base = foot, cycle_commute_base = bicycle, drive_commute_base = car_driver) %>%
  mutate(across(all_commute_base:other, smart.round)) %>% 
  filter(all_commute_base > 0)

desire_lines_20km = desire_lines_rounded %>% 
  filter(length <= max_length)

desire_lines_threshold = desire_lines_rounded %>%
  filter(all_commute_base >= min_flow_routes)

desire_lines_bounding = desire_lines_20km %>% 
  filter(geo_code2 %in% desire_lines_threshold$geo_code2)

# Large study area MSOAs --------------------------------------------------
large_study_area = sf::st_convex_hull(sf::st_union(desire_lines_bounding))
large_study_area = stplanr::geo_buffer(large_study_area, dist = large_area_buffer)

dsn = file.path("data-small", site_name, "large-study-area.geojson")
file.remove(dsn)
sf::write_sf(large_study_area, dsn = dsn)

desire_lines_many = desire_lines_rounded[large_study_area, , op = sf::st_within]

desire_lines_many = desire_lines_many %>% 
  select(geo_code1, geo_code2, all_commute_base, walk_commute_base, cycle_commute_base, drive_commute_base)

# Create routes and generate Go Dutch scenario ---------------------
cl = parallel::makeCluster(parallel::detectCores())

routes_fast = stplanr::route(l = desire_lines_many, route_fun = cyclestreets::journey, cl = cl)
routes_balanced = stplanr::route(l = desire_lines_many, route_fun = cyclestreets::journey, cl = cl, plan = "balanced")
routes_quiet = stplanr::route(l = desire_lines_many, route_fun = cyclestreets::journey, cl = cl, plan = "quietest")
routes_walk = stplanr::route(l = desire_lines_many, route_fun = stplanr::route_osrm, cl = cl)

# routes_fast$busyness = routes_fast$busynance / routes_fast$distances

routes_fast = routes_fast %>%
  mutate(busyness = busynance / distances) %>% 
  group_by(geo_code1, geo_code2) %>%
  mutate(
    n = n(), #could remove
    # all_commute_base = mean(all_commute_base),
    # cycle_commute_base = mean(cycle_commute_base),
    mean_gradient = weighted.mean(gradient_smooth, distances),
    max_gradient = max(gradient_smooth),
    distance_m = sum(distances),
    mean_busyness = weighted.mean(busyness, distances),
    max_busyness = max(busyness)
  ) %>%
  ungroup() %>% 
  mutate(
    pcycle_commute_godutch = pct::uptake_pct_godutch_2020(distance = distance_m, gradient = mean_gradient),
    cycle_commute_godutch = pcycle_commute_godutch * all_commute_base,
    across(c(mean_gradient, max_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_commute_godutch), round, 6)
  )

# routes_fast = routes_fast %>%
#   mutate(across(c(mean_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_commute_godutch), round, 6))

# to round cycle_commute_godutch
routes_fast_summarised = routes_fast %>% 
  st_drop_geometry() %>% 
  group_by(geo_code2) %>% 
  summarise(cycle_commute_godutch = mean(cycle_commute_godutch))
routes_fast_summarised = routes_fast_summarised %>% 
  mutate(cycle_commute_godutch = smart.round(cycle_commute_godutch))

routes_fast = inner_join((routes_fast %>% select(-cycle_commute_godutch)), routes_fast_summarised)

# routes_fast_save = routes_fast %>%
#   select(geo_code1, geo_code2, distance_m, mean_gradient, max_gradient, mean_busyness, max_busyness, all_commute_base, cycle_commute_base, cycle_commute_godutch, busyness, gradient_smooth)

# routes_fast_des = routes_fast %>% 
#   group_by(geo_code2) %>% 
#   summarise(
#     cycle_commute_godutch = mean(cycle_commute_godutch),
#     pcycle_commute_godutch = mean(pcycle_commute_godutch)
#   )

routes_fast_entire = routes_fast %>% 
  group_by(geo_code1, geo_code2, distance_m, mean_gradient, max_gradient, mean_busyness, max_busyness, all_commute_base, cycle_commute_base, cycle_commute_godutch, pcycle_commute_godutch) %>% 
  summarise() %>% 
  arrange(cycle_commute_base)

routes_fast_des = routes_fast_entire %>% 
  select(geo_code2, cycle_commute_godutch, pcycle_commute_godutch)

dsn = file.path("data-small", site_name, "routes-fast.geojson")
obj = routes_fast_entire %>%  select(-pcycle_commute_godutch)
file.remove(dsn)
sf::write_sf(obj = obj, dsn = dsn)

# Walking routes
routes_walk = routes_walk %>% 
  filter(distance <= 6000) %>%
  mutate(
    pwalk_commute_base = walk_commute_base / all_commute_base,
    pwalk_commute_godutch = case_when(	
      distance <= 3000 ~ pwalk_commute_base + 0.1, # 10% shift walking for routes >3km
      distance <= 6000 ~ pwalk_commute_base + 0.05, # 5% shift walking for routes 3-6km	
      TRUE ~ pwalk_commute_base),
    walk_commute_godutch = pwalk_commute_godutch * all_commute_base
  ) 

routes_walk = routes_walk %>%
  mutate(walk_commute_godutch = smart.round(walk_commute_godutch)) %>%
  mutate(across(c(pwalk_commute_base:pwalk_commute_godutch), round, 6))

routes_walk_save = routes_walk %>%
  select(geo_code1, geo_code2, distance, duration, all_commute_base, walk_commute_base, walk_commute_godutch)

dsn = file.path("data-small", site_name, "routes-walk.geojson")
file.remove(dsn)
sf::write_sf(routes_walk_save, dsn = dsn)

# Route networks ----------------------------------------------------------
rnet_fast = overline(routes_fast, attrib = c("cycle_commute_base", "cycle_commute_godutch", "busyness", "gradient_smooth"), fun = c(sum, mean))
rnet_fast = rnet_fast %>% 
  select(cycle_commute_base = cycle_commute_base_fn1, cycle_commute_godutch = cycle_commute_godutch_fn1, busyness = busyness_fn2, gradient_smooth = gradient_smooth_fn2) %>% 
  mutate(gradient_smooth = round(gradient_smooth, 6))
nrow(rnet_fast)
# mapview::mapview(rnet_fast["cycle_commute_base"])

dsn = file.path("data-small", site_name, "rnet-fast.geojson")
file.remove(dsn)
sf::write_sf(rnet_fast, dsn = dsn)

rnet_balanced = overline(routes_balanced_save, attrib = c("cycle_commute_base", "cycle_commute_godutch", "busyness"), fun = c(sum, mean))
rnet_balanced = rnet_balanced %>% 
  select(cycle_commute_base = cycle_commute_base_fn1, cycle_commute_godutch = cycle_commute_godutch_fn1, busyness = busyness_fn2)

dsn = file.path("data-small", site_name, "rnet-balanced.geojson")
file.remove(dsn)
sf::write_sf(rnet_balanced, dsn = dsn)

rnet_quiet = overline(routes_quiet_save, attrib = c("cycle_commute_base", "cycle_commute_godutch", "busyness"), fun = c(sum, mean))
rnet_quiet = rnet_quiet %>% 
  select(cycle_commute_base = cycle_commute_base_fn1, cycle_commute_godutch = cycle_commute_godutch_fn1, busyness = busyness_fn2)

dsn = file.path("data-small", site_name, "rnet-quiet.geojson")
file.remove(dsn)
sf::write_sf(rnet_quiet, dsn = dsn)

# r_walk_grouped_lines = routes_walk_save %>% st_cast("LINESTRING") #is this needed?
rnet_walk = overline(routes_walk_save, attrib = c("walk_commute_base", "walk_commute_godutch", "duration"), fun = c(sum, mean))
rnet_walk = rnet_walk %>% 
  select(walk_commute_base = walk_commute_base_fn1, walk_commute_godutch = walk_commute_godutch_fn1, duration = duration_fn2)

dsn = file.path("data-small", site_name, "rnet-walk.geojson")
file.remove(dsn)
sf::write_sf(rnet_walk, dsn = dsn)

# Go Dutch scenario for desire lines -------------------------------------
join_fast = routes_fast_des %>% 
  st_drop_geometry() %>% 
  select(geo_code2, cycle_commute_godutch, pcycle_commute_godutch)
desire_lines_scenario = inner_join(desire_lines_many, join_fast, by = "geo_code2")
join_walk = routes_walk %>% 
  st_drop_geometry() %>% 
  select(geo_code2, walk_commute_godutch, pwalk_commute_godutch)
desire_lines_scenario = inner_join(desire_lines_scenario, join_walk, by = "geo_code2")

# todo: estimate which proportion of the new walkers/cyclists in the go dutch scenarios would switch from driving, and which proportion would switch from other modes
desire_lines_scenario = desire_lines_scenario %>% 
  mutate(
    drive_commute_godutch = case_when(
      drive_commute_base + (cycle_commute_base - cycle_commute_godutch) + (walk_commute_base - walk_commute_godutch) >= 0 ~ 
        drive_commute_base + (cycle_commute_base - cycle_commute_godutch) + (walk_commute_base - walk_commute_godutch),
      TRUE ~ 0
    )
    # ,
    # pwalk_commute_base = walk_commute_base / all_commute_base,
    # pcycle_commute_base = cycle_commute_base / all_commute_base,
    # pdrive_commute_base = drive_commute_base / all_commute_base,
    # pdrive_commute_godutch = drive_commute_godutch / all_commute_base
    ) %>%
  select(
    geo_code1:drive_commute_base, walk_commute_godutch, cycle_commute_godutch, drive_commute_godutch
    # , pwalk_commute_base:pdrive_commute_base, pwalk_commute_godutch, pcycle_commute_godutch, pdrive_commute_godutch
    ) #%>% 
  # mutate(across(pwalk_commute_base:pdrive_commute_godutch, round, 6))

dsn = file.path("data-small", site_name, "desire-lines-many.geojson")
file.remove(dsn)
sf::write_sf(desire_lines_scenario, dsn = dsn)

# Get region of interest from desire lines --------------------------------
min_flow_map = site_population / 80
desire_lines_busy = desire_lines_scenario %>% 
  filter(all_commute_base >= min_flow_map)

convex_hull = sf::st_convex_hull(sf::st_union(desire_lines_busy))
study_area = stplanr::geo_buffer(convex_hull, dist = region_buffer_dist)
st_precision(study_area) = 1000000

to_add = site %>% 
  st_drop_geometry()
study_area = st_sf(cbind(to_add, study_area))

# zones_touching_small_study_area = zones_msoa_national[study_area, , op = sf::st_intersects]

dsn = file.path("data-small", site_name, "small-study-area.geojson")
file.remove(dsn)
sf::write_sf(study_area, dsn = dsn)

# Desire lines for small study area ---------------------------------------
desire_lines_few = desire_lines_scenario[study_area, , op = sf::st_within]
# desire_lines_few = desire_lines_few %>% 
#   select(geo_code1, geo_code2, all_commute_base, walk_commute_base, cycle_commute_base, drive_commute_base, walk_commute_godutch:drive_commute_godutch)

dsn = file.path("data-small", site_name, "desire-lines-few.geojson")
file.remove(dsn)
sf::write_sf(desire_lines_few, dsn = dsn)


# Get LSOA level JTS data for site ----------------------------------------
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

