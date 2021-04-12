# Aim: adding a new site to existing developments

site = sf::read_sf("data-small/exeter-red-cow-village/exeter-red-cow-village.geojson") 
# todo should have these columns prepopulated
site_name = "exeter-red-cow-village"
site$site_name = site_name
site$full_name = "Exeter Red Cow Village (Liveable Exeter)"
site$main_local_authority = "Mid Devon"
site$is_complete = "No"
site$planning_url = "https://www.liveableexeter.co.uk/garden-communities/garden-communities/red-cow-village/"
site$dwellings_when_complete = 664
site$percent_commute_walk_base = NA
site$percent_commute_cycle_base = NA
site$percent_commute_drive_base = NA
site$percent_commute_bus_base = NA
site$percent_commute_rail_base = NA
site$percent_commute_other_base = NA
site$median_commute_distance = NA
site$distance_to_town = NA
site$percent_commute_active_base = NA
site$percent_drive_convertable = NA
site$percent_mapped_drive_convertable = NA
site$percent_scenario_active = NA
site$percent_commute_active_scenario = NA
site$percent_commute_drive_scenario = NA
site$percent_commute_walk_scenario = NA
site$percent_commute_cycle_scenario = NA
site$crossing_points = NA

f = paste0("data-small/", site_name, "/desire-lines-many.geojson")
desire_lines = sf::read_sf(f)
desire_lines = desire_lines %>% filter(purpose == "commute") # take out journeys to towns
f = paste0("data-small/", site_name, "/all-census-od.csv")
all_desire_lines = read_csv(f)

site_boundary = site$geometry[site$site_name == site_name]
site_line = st_cast(site_boundary,"LINESTRING")

f = paste0("data-small/", site_name, "/routes-fast.geojson")
fast_routes = sf::read_sf(f)
route_town = fast_routes %>% filter(purpose == "town")

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
percent_scenario_active = round(100 * (active_base + active_dutch - active_near) / all_trips)
# to correct for missing desire lines in small sites: calculate the % by which active travel has increased in the mapped desire lines, then assume it increases by the same proportion in unmapped desire lines (yes this is slightly optimistic because the unmapped ones will be longer, but at least it evens things out between sites with different populations) 
percent_commute_active_increase = active_dutch / active_near
percent_commute_active_scenario = (active_base / all_trips * percent_commute_active_increase)
percent_commute_drive_scenario = round(100 * (percent_trimode_trips - percent_commute_active_scenario))
percent_commute_walk_scenario = round(100 * (walk_dutch / active_dutch * percent_commute_active_scenario))
percent_commute_cycle_scenario = round(100 * (cycle_dutch / active_dutch * percent_commute_active_scenario))

# code to re-add the data to the site table
site$percent_commute_walk_base = percent_commute_walk_base
site$percent_commute_cycle_base= percent_commute_cycle_base
site$percent_commute_drive_base= percent_commute_drive_base
site$percent_commute_bus_base= percent_commute_bus_base
site$percent_commute_rail_base= percent_commute_rail_base
site$percent_commute_other_base= percent_commute_other_base

site$distance_to_town= round(route_town$length / 1000, 1)
site$median_commute_distance= median_dist
site$percent_commute_active_base= percent_commute_active_base
site$percent_drive_convertable= pchanged
site$percent_mapped_drive_convertable= pchanged_ofnear
site$percent_scenario_active= percent_scenario_active
site$percent_commute_active_scenario= round(100 * percent_commute_active_scenario)
site$percent_commute_drive_scenario= percent_commute_drive_scenario
site$percent_commute_walk_scenario= percent_commute_walk_scenario
site$percent_commute_cycle_scenario= percent_commute_cycle_scenario


data_dir = "data-small" # for test sites
max_length = 20000 # maximum length of desire lines in m
household_size = 2.3 # mean UK household size at 2011 census
min_flow_routes = 10 # threshold above which OD pairs are included
region_buffer_dist = 2000
large_area_buffer = 500
site_population = site$dwellings_when_complete * household_size


sites = sf::read_sf("data-small/all-sites.geojson")


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

# site_all_base = sum(desire_lines_combined$all)
# site_walk_base = sum(desire_lines_combined$foot) / site_all_base
# site_cycle_base = sum(desire_lines_combined$bicycle) / site_all_base
# site_drive_base = sum(desire_lines_combined$car_driver) / site_all_base
# site_bus_base = sum(desire_lines_combined$bus) / site_all_base
# site_rail_base = (sum(desire_lines_combined$train) + sum(desire_lines_combined$light_rail)) / site_all_base
# site_other_base = (sum(desire_lines_combined$car_passenger) +sum(desire_lines_combined$taxi) + sum(desire_lines_combined$motorbike) + sum(desire_lines_combined$other)) / site_all_base

site_data = site
# site_data$pwalk = site_walk_base
# site_data$pcycle = site_cycle_base
# site_data$pdrive = site_drive_base
# site_data$pbus = site_bus_base
# site_data$prail = site_rail_base
# site_data$pother = site_other_base
# site_data = site_data %>% 
#   mutate(across(pwalk:pother, round, 2))

site_data = inner_join(site_data, st_drop_geometry(access_means), by = "site_name")
st_precision(site_data) = 1000000


dsn = file.path(data_dir, site_name, "site.geojson")
if(file.exists(dsn)) file.remove(dsn)
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



setdiff(names(sites),names(site))

s <- names(sites)%in%names(site)

names(site)[!s]

sites = rbind(sites,site_data)

dsn = file.path(data_dir, site_name, "jts-lsoas.geojson")
if(file.exists(dsn)) file.remove(dsn)
write_sf(lsoas_all, dsn = dsn)
