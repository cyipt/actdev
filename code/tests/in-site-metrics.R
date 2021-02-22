# Aim: calculate in-site metrics

# Question: should we base this on OSM data or routing data?
# It's quicker and less resource intensive to calculate 
# these metrics using a simple routing approach, reducing
# the computational requirements associated with bulk
# import and processing of OSM, although that 2nd 
# approach allows more detailed info to be extracted


# Approach1: quicker and easier -------------------------------------------

# get starting point from infographics.R
# file.edit("code/infographics.R")
library(tidyverse)

if(!exists("site_name")) site_name = "great-kneighton"
sites = sf::read_sf("data-small/all-sites.geojson")
site = sites[sites$site_name == site_name, ]
path = file.path("data-small", site_name)

# input data: we should probably have naming conventions for these
list.files(path)
site_area = sf::read_sf(file.path(path, "site.geojson"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-few.geojson"))	
routes_fast = sf::read_sf(file.path(path, "routes-fast.geojson"))
routes_quiet = sf::read_sf(file.path(path, "routes-quiet.geojson"))
routes_walk = readRDS(file.path(path, "routes_walk.Rds"))
mapview::mapview(site_area)

# sample points inside the area for route analysis
set.seed(2021) # reproducible results
n_routes = 20
site_points_origin = sf::st_sample(site_area, n_routes)
site_points_destination = sf::st_sample(site_area, n_routes)
site_odc = cbind(
  sf::st_coordinates(site_points_origin),
  sf::st_coordinates(site_points_destination)
)
site_desire_lines = od::odc_to_sf(site_odc)
mapview::mapview(site_desire_lines)


# Approach 2: use data from osmextract ------------------------------------


