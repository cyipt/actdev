
# r_fast_grouped_lines = routes_fast_save %>% st_cast("LINESTRING") #wouldn't be needed
nrow(routes_fast_save)
mapview::mapview(routes_fast_save)

# breaks-up linestrings too much due to overlapping routes
# routes_fast_breakup = stplanr::rnet_breakup_vertices(routes_fast_save, verbose = T) # how is that function so fast!
# nrow(routes_fast_breakup) / nrow(routes_fast_save) # There are ~5 times more segments
routes_fast_save$geometry_txt = sf::st_as_text(routes_fast_save$geometry)
rnet_fast_1 = routes_fast_save %>% 
  sf::st_drop_geometry() %>% 
  group_by(geometry_txt) %>% 
  summarise(cycle_commute_base = sum(cycle_commute_base))
rnet_fast_1_sf = sf::st_sf(
  rnet_fast_1 %>% select(-geometry_txt),
  geometry = sf::st_as_sfc(rnet_fast_1$geometry_txt), crs = 4326
)
nrow(rnet_fast_1_sf)
rnet_fast_breakup = stplanr::rnet_breakup_vertices(rnet_fast_1_sf)
nrow(rnet_fast_breakup)
nrow(rnet_fast_breakup) / nrow(rnet_fast_1_sf) # 1.5 - more rational
mapview::mapview(rnet_fast_breakup)


rnet_fast_breakup$geometry_txt = sf::st_as_text(rnet_fast_breakup$geometry)
rnet_fast_2 = rnet_fast_breakup %>% 
  sf::st_drop_geometry() %>% 
  group_by(geometry_txt) %>% 
  summarise(cycle_commute_base = sum(cycle_commute_base))
rnet_fast_2_sf = sf::st_sf(
  rnet_fast_2 %>% select(-geometry_txt),
  geometry = sf::st_as_sfc(rnet_fast_2$geometry_txt, crs = 4326)
)

mapview::mapview(rnet_fast_2_sf)
nrow(rnet_fast_2_sf)

nrow(routes_fast_breakup) / nrow(routes_fast_breakup_unique) # 2.8 duplications

routes_fast_breakup$distance_segment = geo_length(routes_fast_breakup)
sum(st_length(routes_fast_save))
sum(routes_fast_breakup$distance_segment) # identical: sanity check

# commented out because it's slow and clunky, kept for now but this can be deleted (RL)
# routes_fast_grouped_seg = routes_fast_breakup %>% 
#   group_by(geometry_txt) %>% 
#   summarise(distance_m = mean(distance_m))

# new way of creating route networks, see:
# https://github.com/ropensci/stplanr/issues/435
system.time({
  
  rnet_fast = routes_fast_breakup %>% 
    # group_by(geometry) %>% # currently fails 
    sf::st_drop_geometry() %>%
    group_by(geometry_txt) %>%
    summarise(cycle_commute_base = sum(cycle_commute_base))
  
  # testing faster implemetation, can be deleted (RL)
  # remotes::install_cran("wk")
  # rnet_fast_geometry = wk::wkt(rnet_fast$geometry_txt)
  # see: https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
  rnet_fast_new = sf::st_sf(rnet_fast, geometry = sf::st_as_sfc(rnet_fast$geometry_txt), crs = 4326) %>% 
    select(-geometry_txt)
  
})

# same result as overline
mapview::mapview(rnet_fast_new)

# old rnet code (RL)
system.time({
  rnet_fast = overline(routes_fast_save, attrib = c("cycle_commute_base", "busyness"), )
})
