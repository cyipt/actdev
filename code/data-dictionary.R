library(sf)

jts = read_sf("data-small/chapelford/site.geojson") %>% 
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

write_csv(data_dictionary, "data-small/site-data-dictionary.csv")

# Desire lines
des = read_sf("data-small/chapelford/desire-lines-many.geojson") %>% 
  st_drop_geometry()
names = colnames(des)
data_dictionary_des = data.frame(names)
data_dictionary_des$description = c(
  "origin MSOA code representing the site",
  "destination MSOA code",
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
  "All journeys",
  "Journeys by foot, baseline scenario",
  "Journeys by bicycle, baseline scenario",
  "Journeys by car drivers, baseline scenario",
  "Journeys by foot, Go Dutch scenario",
  "Journeys by bicycle, Go Dutch scenario",
  "Journeys by car drivers, Go Dutch scenario"
)

write_csv(data_dictionary_des, "data-small/desire-line-data-dictionary.csv")

# Route networks
rnet = read_sf("data-small/chapelford/rnet-fast.geojson") %>% 
  st_drop_geometry()
names = colnames(rnet)
data_dictionary_rnet = data.frame(names)
data_dictionary_rnet$description = c(
  "origin MSOA code representing the site",
  "destination MSOA code",
  "all journeys from origin to destination (baseline and Go Dutch scenarios)",
  "journeys by foot from origin to destination in baseline scenario",
  "journeys by bicycle from origin to destination in baseline scenario",
  "journeys by car drivers from origin to destination in baseline scenario",
  "journeys by foot from origin to destination in Go Dutch scenario",
  "journeys by bicycle from origin to destination in Go Dutch scenario",
  "journeys by car drivers from origin to destination in Go Dutch scenario"
)
data_dictionary_rnet$title = c(
  "Origin",
  "Destination",
  "All journeys",
  "Journeys by foot, baseline scenario",
  "Journeys by bicycle, baseline scenario",
  "Journeys by car drivers, baseline scenario",
  "Journeys by foot, Go Dutch scenario",
  "Journeys by bicycle, Go Dutch scenario",
  "Journeys by car drivers, Go Dutch scenario"
)

write_csv(data_dictionary_rnet, "data-small/rnet-data-dictionary.csv")

# Routes
rou = read_sf("data-small/chapelford/routes-fast.geojson") %>% 
  st_drop_geometry()
names = colnames(rou)
data_dictionary_rou = data.frame(names)
data_dictionary_rou$description = c(
  "origin MSOA code representing the site",
  "destination MSOA code",
  "all journeys from origin to destination (baseline and Go Dutch scenarios)",
  "journeys by foot from origin to destination in baseline scenario",
  "journeys by bicycle from origin to destination in baseline scenario",
  "journeys by car drivers from origin to destination in baseline scenario",
  "journeys by foot from origin to destination in Go Dutch scenario",
  "journeys by bicycle from origin to destination in Go Dutch scenario",
  "journeys by car drivers from origin to destination in Go Dutch scenario"
)
data_dictionary_rou$title = c(
  "Origin",
  "Destination",
  "All journeys",
  "Journeys by foot, baseline scenario",
  "Journeys by bicycle, baseline scenario",
  "Journeys by car drivers, baseline scenario",
  "Journeys by foot, Go Dutch scenario",
  "Journeys by bicycle, Go Dutch scenario",
  "Journeys by car drivers, Go Dutch scenario"
)

write_csv(data_dictionary_rou, "data-small/routes-data-dictionary.csv")

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

write_csv(data_dictionary_od, "data-small/all-census-od-data-dictionary.csv")
