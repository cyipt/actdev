library(sf)
library(tidyverse)

jts = read_sf("data-small/great-kneighton/site.geojson") %>% 
  st_drop_geometry()
names = colnames(jts)
data_dictionary = data.frame(names)
data_dictionary$description = c(
  "name of site",
  "estimated number of dwellings at completion",
  "average minimum travel time to centres of employment by walking/public transport (weighted mean of travel times to LSOAs with 100-499, 500-4999 and 5000+ jobs; using population weighted mean of the minimum journey times from each OA centroid)", 
  "average minimum travel time to centres of employment by cycling (weighted mean of travel times to LSOAs with >100, >500 and >5000 jobs; using population weighted mean of the minimum journey times from each OA centroid)", 
  "average minimum travel time to centres of employment by car (weighted mean of travel times to LSOAs with >100, >500 and >5000 jobs; using population weighted mean of the minimum journey times from each OA centroid)", 
  "average minimum travel time to primary schools by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to primary schools by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to primary schools by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to secondary schools by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to secondary schools by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to secondary schools by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to further education colleges by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to further education colleges by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to further education colleges by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to doctors' surgeries by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to doctors' surgeries by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to doctors' surgeries by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to hospitals by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to hospitals by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to hospitals by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to food stores by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to food stores by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to food stores by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to town centres by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to town centres by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to town centres by car (using population weighted mean of the minimum journey times from each OA centroid)"
)
data_dictionary$title = c(
  "Site name",
  "Number of dwellings",
  "Travel time to nearest employment centre by foot/public transport",
  "Travel time to nearest employment centre by bicycle",
  "Travel time to nearest employment centre by car",
  "Travel time to nearest primary school by foot/public transport",
  "Travel time to nearest primary school by bicycle",
  "Travel time to nearest primary school by car",
  "Travel time to nearest secondary school by foot/public transport",
  "Travel time to nearest secondary school by bicycle",
  "Travel time to nearest secondary school by car",
  "Travel time to nearest FE college by foot/public transport",
  "Travel time to nearest FE college by bicycle",
  "Travel time to nearest FE college by car",
  "Travel time to nearest GP surgery by foot/public transport",
  "Travel time to nearest GP surgery by bicycle",
  "Travel time to nearest GP surgery by car",
  "Travel time to nearest hospital by foot/public transport",
  "Travel time to nearest hospital by bicycle",
  "Travel time to nearest hospital by car",
  "Travel time to nearest food store by foot/public transport",
  "Travel time to nearest food store by bicycle",
  "Travel time to nearest food store by car",
  "Travel time to nearest town centre by foot/public transport",
  "Travel time to nearest town centre by bicycle",
  "Travel time to nearest town centre by car"
)

file.remove("data-small/site-data-dictionary.csv")
write_csv(data_dictionary, "data-small/site-data-dictionary.csv")

# Desire lines
des = read_sf("data-small/great-kneighton/desire-lines-many.geojson") %>% 
  st_drop_geometry()
names = colnames(des)
data_dictionary_des = data.frame(names)
data_dictionary_des$description = c(
  "origin MSOA code representing the site",
  "destination MSOA code",
  "journey purpose (commute/town/supermarket)",
  "Euclidean distance (m)",
  "all journeys from origin to destination (baseline and Go Dutch scenarios)",
  "journeys by foot from origin to destination in baseline scenario",
  "journeys by bicycle from origin to destination in baseline scenario",
  "journeys by car drivers from origin to destination in baseline scenario",
  "journeys by foot from origin to destination in Go Dutch scenario",
  "journeys by bicycle from origin to destination in Go Dutch scenario",
  "journeys by car drivers from origin to destination in Go Dutch scenario"
)
data_dictionary_des$title = c(
  "Origin",
  "Destination",
  "Purpose",
  "Distance (m)",
  "All journeys",
  "Journeys by foot, baseline scenario",
  "Journeys by bicycle, baseline scenario",
  "Journeys by car drivers, baseline scenario",
  "Journeys by foot, Go Dutch scenario",
  "Journeys by bicycle, Go Dutch scenario",
  "Journeys by car drivers, Go Dutch scenario"
)

file.remove("data-small/desire-line-data-dictionary.csv")
write_csv(data_dictionary_des, "data-small/desire-line-data-dictionary.csv")

# Route networks cycle
rnet = read_sf("data-small/great-kneighton/rnet-fast.geojson") %>% 
  st_drop_geometry()
names = colnames(rnet)
data_dictionary_rnet = data.frame(names)
data_dictionary_rnet$description = c(
  "journeys by bicycle along route segment in baseline scenario",
  "journeys by bicycle along route segment in Go Dutch scenario",
  "busyness of route segment",
  "smoothed gradient of route segment"
)
data_dictionary_rnet$title = c(
  "Journeys by bicycle, baseline scenario",
  "Journeys by bicycle, Go Dutch scenario",
  "Road busyness",
  "Gradient"
)

file.remove("data-small/rnet-cycle-data-dictionary.csv")
write_csv(data_dictionary_rnet, "data-small/rnet-cycle-data-dictionary.csv")

# Routes cycle
rou = read_sf("data-small/great-kneighton/routes-fast.geojson") %>% 
  st_drop_geometry()
names = colnames(rou)
data_dictionary_rou = data.frame(names)
data_dictionary_rou$description = c(
  "origin MSOA code representing the site",
  "destination MSOA code",
  "journey purpose (commute/town/supermarket)",
  "total length of route (m)",
  "distance weighted mean gradient of route segments",
  "maximum route segment gradient",
  "distance weighted mean busyness of route segments",
  "maximum route segment busyness",
  "all journeys from origin to destination (baseline and Go Dutch scenarios)",
  "journeys by bicycle from origin to destination in baseline scenario",
  "journeys by bicycle from origin to destination in Go Dutch scenario"
)
data_dictionary_rou$title = c(
  "Origin",
  "Destination",
  "Purpose",
  "Route length (m)",
  "Mean gradient",
  "Maximum gradient", 
  "Mean road busyness",
  "Maximum road busyness",
  "All journeys",
  "Journeys by bicycle, baseline scenario",
  "Journeys by bicycle, Go Dutch scenario"
)

file.remove("data-small/routes-cycle-data-dictionary.csv")
write_csv(data_dictionary_rou, "data-small/routes-cycle-data-dictionary.csv")

# Route networks walk
rnet_walk_dic = read_sf("data-small/great-kneighton/rnet-walk.geojson") %>% 
  st_drop_geometry()
names = colnames(rnet_walk_dic)
data_dictionary_rnet_walk = data.frame(names)
data_dictionary_rnet_walk$description = c(
  "journeys by foot along route segment in baseline scenario",
  "journeys by foot along route segment in Go Dutch scenario",
  "time taken to walk length of route segment"
)
data_dictionary_rnet_walk$title = c(
  "Journeys by foot, baseline scenario",
  "Journeys by foot, Go Dutch scenario",
  "Time taken"
)

file.remove("data-small/rnet-walk-data-dictionary.csv")
write_csv(data_dictionary_rnet_walk, "data-small/rnet-walk-data-dictionary.csv")

# Routes walk
rou_walk = read_sf("data-small/great-kneighton/routes-walk.geojson") %>% 
  st_drop_geometry()
names = colnames(rou_walk)
data_dictionary_rou_walk = data.frame(names)
data_dictionary_rou_walk$description = c(
  "origin MSOA code representing the site",
  "destination MSOA code",
  "journey purpose (commute/town/supermarket)",
  "total length of route (m)",
  "total time taken for route",
  "all journeys from origin to destination (baseline and Go Dutch scenarios)",
  "journeys by foot from origin to destination in baseline scenario",
  "journeys by foot from origin to destination in Go Dutch scenario"
)
data_dictionary_rou_walk$title = c(
  "Origin",
  "Destination",
  "Purpose",
  "Route length (m)",
  "Time taken",
  "All journeys",
  "Journeys by foot, baseline scenario",
  "Journeys by foot, Go Dutch scenario"
)

file.remove("data-small/routes-walk-data-dictionary.csv")
write_csv(data_dictionary_rou_walk, "data-small/routes-walk-data-dictionary.csv")


# OD csv
od = read_csv("data-small/great-kneighton/all-census-od.csv") %>% 
  select(-geometry)
names = colnames(od)
data_dictionary_od = data.frame(names)
data_dictionary_od$description = c(
  "origin MSOA code representing the site",
  "destination MSOA code",
  "all commuter journeys from origin to destination; based on 2011 census flows adjusted to site population",
  "employed population who live in origin MSOA, work in destination MSOA, and work from home; based on 2011 census flows adjusted to site population",
  "commuter journeys by light rail from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by train from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by bus from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by taxi from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by motorbike from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by car drivers from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by car passengers from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by bicycle from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by foot from origin to destination; based on 2011 census flows adjusted to site population",
  "commuter journeys by other mode from origin to destination; based on 2011 census flows adjusted to site population",
  "length of desire line (Euclidean distance, m)",
  "proportion of commuter journeys from origin to destination undertaken by foot; based on 2011 census flows",
  "proportion of commuter journeys from origin to destination undertaken by bicycle; based on 2011 census flows",
  "proportion of commuter journeys from origin to destination undertaken by car drivers; based on 2011 census flows"
)
data_dictionary_od$title = c(
  "Origin",
  "Destination",
  "All commuter journeys",
  "Working from home, baseline scenario",
  "Commutes by light rail, baseline scenario",
  "Commutes by train, baseline scenario",
  "Commutes by bus, baseline scenario",
  "Commutes by taxi, baseline scenario",
  "Commutes by motorbike, baseline scenario",
  "Commutes by car drivers, baseline scenario",
  "Commutes by car passengers, baseline scenario",
  "Commutes by bicycle, baseline scenario",
  "Commutes by foot, baseline scenario",
  "Commutes by other mode, baseline scenario",
  "Distance (m)",
  "Proportion of commutes by foot",
  "Proportion of commutes by bicycle",
  "Proportion of commutes by car drivers"
)

file.remove("data-small/all-census-od-data-dictionary.csv")
write_csv(data_dictionary_od, "data-small/all-census-od-data-dictionary.csv")

# jts lsoas
jts_lsoas = read_sf("data-small/great-kneighton/jts-lsoas.geojson") %>% 
  st_drop_geometry()
names = colnames(jts_lsoas)
data_dictionary_lsoas = data.frame(names)
data_dictionary_lsoas$description = c(
  "LSOA code (all LSOAs within large study area)",
  "average minimum travel time to centres of employment by walking/public transport (weighted mean of travel times to LSOAs with 100-499, 500-4999 and 5000+ jobs; using population weighted mean of the minimum journey times from each OA centroid)", 
  "average minimum travel time to centres of employment by cycling (weighted mean of travel times to LSOAs with >100, >500 and >5000 jobs; using population weighted mean of the minimum journey times from each OA centroid)", 
  "average minimum travel time to centres of employment by car (weighted mean of travel times to LSOAs with >100, >500 and >5000 jobs; using population weighted mean of the minimum journey times from each OA centroid)", 
  "average minimum travel time to primary schools by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to primary schools by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to primary schools by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to secondary schools by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to secondary schools by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to secondary schools by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to further education colleges by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to further education colleges by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to further education colleges by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to doctors' surgeries by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to doctors' surgeries by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to doctors' surgeries by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to hospitals by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to hospitals by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to hospitals by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to food stores by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to food stores by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to food stores by car (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to town centres by walking/public transport (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to town centres by cycling (using population weighted mean of the minimum journey times from each OA centroid)",
  "average minimum travel time to town centres by car (using population weighted mean of the minimum journey times from each OA centroid)"
)
data_dictionary_lsoas$title = c(
  "LSOA code",
  "Travel time to nearest employment centre by foot/public transport",
  "Travel time to nearest employment centre by bicycle",
  "Travel time to nearest employment centre by car",
  "Travel time to nearest primary school by foot/public transport",
  "Travel time to nearest primary school by bicycle",
  "Travel time to nearest primary school by car",
  "Travel time to nearest secondary school by foot/public transport",
  "Travel time to nearest secondary school by bicycle",
  "Travel time to nearest secondary school by car",
  "Travel time to nearest FE college by foot/public transport",
  "Travel time to nearest FE college by bicycle",
  "Travel time to nearest FE college by car",
  "Travel time to nearest GP surgery by foot/public transport",
  "Travel time to nearest GP surgery by bicycle",
  "Travel time to nearest GP surgery by car",
  "Travel time to nearest hospital by foot/public transport",
  "Travel time to nearest hospital by bicycle",
  "Travel time to nearest hospital by car",
  "Travel time to nearest food store by foot/public transport",
  "Travel time to nearest food store by bicycle",
  "Travel time to nearest food store by car",
  "Travel time to nearest town centre by foot/public transport",
  "Travel time to nearest town centre by bicycle",
  "Travel time to nearest town centre by car"
)

file.remove("data-small/jts-lsoas-data-dictionary.csv")
write_csv(data_dictionary_lsoas, "data-small/jts-lsoas-data-dictionary.csv")

