library(pct)
library(tidyverse)
library(sf)
library(od)
library(acton)


# OA to Workplace Zone flows ----------------------------------------------


u = "https://www.nomisweb.co.uk/output/census/2011/wf02ew_oa.zip"
oa_wpz_flows = get_od(u = u)
oa_wpz_flows = oa_wpz_flows %>%
  rename(oa_code = `Area of residence`, wpz_code = `Area of workplace`)

oa_centroids = sf::read_sf("https://opendata.arcgis.com/datasets/b0c86eaafc5a4f339eb36785628da904_0.geojson")
oa_centroids = oa_centroids %>%
  select(OA11CD)

wpz_centroids = ukboundaries::duraz("https://opendata.arcgis.com/datasets/176661b9403a4c84ae6aedf8bb4127cf_0.zip")
wpz_centroids = wpz_centroids %>%
  select(wz11cd)

flows_filtered = oa_wpz_flows %>%
  filter(wpz_code %in% wpz_centroids$wz11cd)

oa_sample = sample_frac(flows_filtered, 0.01)

summary(oa_sample$wpz_code %in% wpz_centroids$wz11cd)
od_flows = od::od_coordinates(x = oa_sample, p = oa_centroids, pd = wpz_centroids)

od_flows_sf = od::od_to_sf(x = oa_sample, oa_centroids, wpz_centroids)

plot(od_flows_sf$geometry)



# MSOA to MSOA flows ------------------------------------------------------

# in get_od(), geo_code1 is always the home and geo_code2 the place of work
# we are rerouting these flows to take advantage of recent changes in OSM
# get_od() also includes cross-boundary flows, and flows >20km in length

msoa_flows = get_od()

msoa_centroids = get_centroids_ew() %>%
  st_transform(4326)

summary(msoa_flows$geo_code1 %in% msoa_centroids$msoa11cd)
summary(msoa_flows$geo_code2 %in% msoa_centroids$msoa11cd)

# work = unique(msoa_flows$geo_code2)
# home = unique(msoa_flows$geo_code1)
# subset(work, !(work %in% home))

# exclude the following geo_code2:
# OD0000001 = mainly work at or from home
# OD0000002 = offshore installation
# OD0000003 = no fixed place
# OD0000004 = outside UK
# N92000002 = Northern Ireland
# S92000003 = Scotland
msoa_flows_filtered = msoa_flows %>%
  filter(geo_code2 %in% msoa_flows$geo_code1)

# msoa_sample = sample_frac(msoa_flows_filtered, 0.01)
#
#
# od_flows = od::od_coordinates(x = msoa_sample, p = msoa_centroids)
#
# od_flows_sf = od::od_to_sf(x = msoa_sample, z = msoa_centroids)
#
# plot(od_flows_sf$geometry)

# Leeds sites -------------------------------------------------------------

p1 = get_planit_data(bbox = NULL, query_type = "planapplic", query_value = "13/05235/FU@Leeds", base_url = "https://www.planit.org.uk/")
p2 = get_planit_data(bbox = NULL, query_type = "planapplic", query_value = "15/04151/FU@Leeds", base_url = "https://www.planit.org.uk/")
p3 = get_planit_data(bbox = NULL, query_type = "planapplic", query_value = "15/01973/FU@Leeds", base_url = "https://www.planit.org.uk/")
p4 = get_planit_data(bbox = NULL, query_type = "planapplic", query_value = "15/00415/FU@Leeds", base_url = "https://www.planit.org.uk/")


sites = rbind(p1,p2,p3,p4)
sites$place = c("AllertonBywater", "Tyersal", "Micklefield", "LeedsClimateInnovationDistrict")
sites$id = 1:dim(sites)[1]
sites = sites %>%
  select(id, place, everything())

# find the MSOA the site lies within, plus the nearest n MSOA centroids, for a total of 2 MSOAs per site - NEEDS EDITING
c_proj = msoa_centroids %>% st_transform(27700)

msoa_zone = get_pct_zones(region = "west-yorkshire", geography = "msoa") %>%
  select(msoa11cd = geo_code, lad_name)
msoa_zone_leeds = msoa_zone %>%
  filter(lad_name == "Leeds")
z_proj = msoa_zone %>% st_transform(27700)

sites = sites %>%
  st_transform(27700)
# sites = st_join(sites, c_proj, join = st_nearest_feature) %>%
sites = st_join(sites, z_proj, join = st_within) %>%
  st_transform(4326)

# To map zones and centroids
library(mapview)
mapview(msoa_zone) +
  mapview(sites) +
  mapview(msoa_centroids[msoa_centroids$msoa11cd %in% sites$msoa11cd,])

# Get OD data for Leeds sites ---------------------------------------------

# filter flows where the home is in one of the 4 case study MSOAs
msoa_flows_leeds = msoa_flows_filtered %>%
  filter(geo_code1 %in% sites$msoa11cd)

# ensure the origins used are within the new developments
sites_geocodes = sites %>%
  select(msoa11cd)


# filter flows < 20km length
msoa_flows_leeds$id = 1:dim(msoa_flows_leeds)[1]
od_flows_sf = od::od_to_sf(x = msoa_flows_leeds, z = sites_geocodes, zd = msoa_centroids)
od_flows_sf = od_flows_sf %>%
  st_transform(27700) %>%
  mutate(length = st_length(x = od_flows_sf)) %>%
  st_transform(4326)
short_flows_sf = od_flows_sf %>%
  mutate(length = units::drop_units(length)) %>%
  filter(length < 20000)

mapview::mapview(short_flows_sf)

# use filtered dataset to get simple list of coordinate pairs
short_flows_leeds = msoa_flows_leeds %>%
  filter(id %in% short_flows_sf$id)

od_flows = od::od_coordinates(x = short_flows_leeds, p = sites_geocodes, pd = msoa_centroids)

write_csv(od_flows, "od-flows-leeds.csv")




