# JTS data for surrounding LSOAs ------------------------------------------
# inputs: large_study_area, national jts data from set-up script and
lsoa_c = st_centroid(jts0501)
lsoa_c = lsoa_c[large_study_area, ,op = sf::st_within]
lsoas_inside = filter(jts0501, LSOA_code %in% lsoa_c$LSOA_code)

lsoas_bounding = jts0501[desire_lines_bounding, , op = sf::st_intersects]
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

dsn = file.path(data_dir, site_name, "jts-lsoas.geojson")
if(file.exists(dsn)) file.remove(dsn)
write_sf(lsoas_all, dsn = dsn)