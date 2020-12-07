library(pct)
library(tidyverse)
library(sf)
library(od)

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
