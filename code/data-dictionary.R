library(sf)

jts = read_sf("data-small/chapelford/site.geojson") %>% 
  st_drop_geometry()
names = colnames(jts)
data_dictionary = data.frame(names)
data_dictionary$descriptions = c(
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

write_sf(data_dictionary, "data-small/site-data-dictionary.csv")


des = read_sf("data-small/chapelford/desire-lines-many.geojson") %>% 
  st_drop_geometry()
names = colnames(des)
data_dictionary_des = data.frame(names)
data_dictionary_des$descriptions = c(
  "origin MSOA code",
  "destination MSOA code",
  "all commuter journeys from origin to destination",
  "commuter journeys by foot from origin to destination in baseline scenario",
  "commuter journeys by bicycle from origin to destination in baseline scenario",
  "commuter journeys by car drivers from origin to destination in baseline scenario",
  "commuter journeys by foot from origin to destination in Go Dutch scenario",
  "commuter journeys by bicycle from origin to destination in Go Dutch scenario",
  "commuter journeys by car drivers from origin to destination in Go Dutch scenario"
)

write_sf(data_dictionary_des, "data-small/desire-line-data-dictionary.csv")


des = read_sf("data-small/chapelford/all-census-od.csv")
colnames(des)
data_dictionary_des = data.frame(names)
data_dictionary_des$descriptions = c(
  "origin MSOA code",
  "destination MSOA code",
  "all commuter journeys from origin to destination",
  "commuter journeys by foot from origin to destination in baseline scenario",
  "commuter journeys by bicycle from origin to destination in baseline scenario",
  "commuter journeys by car drivers from origin to destination in baseline scenario",
  "commuter journeys by foot from origin to destination in Go Dutch scenario",
  "commuter journeys by bicycle from origin to destination in Go Dutch scenario",
  "commuter journeys by car drivers from origin to destination in Go Dutch scenario"
)

write_sf(data_dictionary_des, "data-small/desire-line-data-dictionary.csv")
