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
library(stplanr)
library(tmap)

# setwd("~/cyipt/actdev/")

if(!exists("site_name")) site_name = "great-kneighton"
if(!exists("sites")){
  sites = sf::read_sf("data-small/all-sites.geojson")
}
site = sites[sites$site_name == site_name, ]
path = file.path("data-small", site_name)

if(site$is_complete != "no") {
# input data: we should probably have naming conventions for these
# list.files(path)
site_area = sf::read_sf(file.path(path, "site.geojson"))
# desire_lines = sf::read_sf(file.path(path, "desire-lines-few.geojson"))	
# routes_fast = sf::read_sf(file.path(path, "routes-fast.geojson"))
# routes_quiet = sf::read_sf(file.path(path, "routes-quiet.geojson"))
# routes_walk = readRDS(file.path(path, "routes_walk.Rds"))
# mapview::mapview(site_area)

# buffer could be used in future, but only if we sort out the problem that walk/cycle/drive routes spawn from different locations (because driving roads must start on a road)
# site_buffer = stplanr::geo_buffer(site_area, dist = 200) 

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
site_desire_lines$length = geo_length(site_desire_lines)
# mapview::mapview(site_desire_lines)

# first generate driving routes
site_routes_drive = route(l = site_desire_lines, route_fun = route_osrm, osrm.profile = "car")

site_routes_drive$drive = 1
site_rnet_drive = overline(site_routes_drive, attrib = "drive")

# then reset start and end points
start_points = lwgeom::st_startpoint(site_routes_drive)
end_points = lwgeom::st_endpoint(site_routes_drive)

site_odc_reset = cbind(
  sf::st_coordinates(start_points),
  sf::st_coordinates(end_points)
)
site_desire_lines_reset = od::odc_to_sf(site_odc_reset)
site_desire_lines_reset$length = geo_length(site_desire_lines_reset)
# mapview::mapview(site_desire_lines_reset)

# then do walking and cycling routes
site_routes_walk = route(l = site_desire_lines_reset, route_fun = route_osrm, osrm.profile = "foot")


site_routes_walk$walk = 1
site_rnet_walk = overline(site_routes_walk, attrib = "walk")

site_routes_cycle = route(l = site_desire_lines_reset, route_fun = route_osrm, osrm.profile = "bike")
# site_routes_cycle = unique(site_routes_cycle) #avoid weird bug in ebbsfleet

site_routes_cycle$cycle = 1
site_rnet_cycle = overline(site_routes_cycle, attrib = "cycle")

site_walk_circuity = sum(site_routes_walk$distance) / sum(site_desire_lines_reset$length)
site_cycle_circuity = sum(site_routes_cycle$distance) / sum(site_desire_lines_reset$length)
site_drive_circuity = sum(site_routes_drive$distance) / sum(site_desire_lines_reset$length)

st_precision(site_rnet_walk) = 1000000
st_precision(site_rnet_cycle) = 1000000
st_precision(site_rnet_drive) = 1000000
st_precision(site_area) = 1000000

dsn = file.path("data-small", site_name, "in-site-walk-rnet.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(site_rnet_walk, dsn)

dsn = file.path("data-small", site_name, "in-site-cycle-rnet.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(site_rnet_cycle, dsn)

dsn = file.path("data-small", site_name, "in-site-drive-rnet.geojson")
if(file.exists(dsn)) file.remove(dsn)
sf::write_sf(site_rnet_drive, dsn)

# infographic plot
# tmap_mode("plot")
# tm_shape(site_area) + tm_polygons() +
#   tm_shape(site_routes_drive) + tm_lines(col = "red", alpha = 0.3) +
#   tm_shape(site_routes_cycle) + tm_lines(col = "green", alpha = 0.3, lwd = 2) +
#   tm_shape(site_routes_walk) + tm_lines(col = "blue", alpha = 0.3, lwd = 2) 

tmap_mode("plot")
site_map = tm_shape(site_area) + tm_polygons() +
  tm_shape(site_rnet_drive) + tm_lines(col = "red", lwd = "drive", scale = 4) +
  tm_shape(site_rnet_cycle) + tm_lines(col = "green", lwd = "cycle", scale = 4) +
  tm_shape(site_rnet_walk) + tm_lines(col = "blue", lwd = "walk", scale = 4)

dsn = file.path("data-small", site_name, "in-site-metrics.png")
tmap_save(site_map, dsn)

in_site = data.frame(site_walk_circuity, site_cycle_circuity, site_drive_circuity)
in_site = round(in_site, 2)

dsn = file.path("data-small", site_name, "in-site-metrics.csv")
write_csv(in_site, dsn)
# Todo: rbind all route lines and create a legend that contains circuity values.

# Approach 2: use data from osmextract ------------------------------------

}
