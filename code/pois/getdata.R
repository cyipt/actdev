

# Define a function to obtain the directory of the script; see: https://stackoverflow.com/a/55322344
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
getCurrentFileLocation <- function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}


# Function to create a POIs dataset
createPoisDataset <- function(area, output_file, osm_key, osm_value)
{
  # Load Simple Features library
  if (!require("sf")) install.packages("sf")
  library(sf)
  
  # Load OSM data library; see: https://github.com/ropensci/osmdata and https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
  if (!require("osmdata")) install.packages("osmdata")
  library(osmdata)
  
  # Load mapview library
  if (!require("mapview")) install.packages("mapview")
  library(mapview)
  
  # Load data.table library
  if (!require("data.table")) install.packages("data.table")
  library(data.table)

  # Load geojsonio library
  if (!require("geojsonio")) install.packages("geojsonio")
  library(geojsonio)
  
  
  # Obtain locations - those entered in OSM as points
  q <- getbb(area) %>%
    opq (nodes_only = TRUE, timeout=25*100) %>%
    add_osm_feature(osm_key, osm_value) %>%
    osmdata_sf()
  pois_frompoints = q$osm_points
  #mapview (pois_frompoints)
  
  # Obtain locations - those entered in OSM as polygons
  q <- getbb(area) %>%
    opq (timeout=25*100) %>%
    add_osm_feature(osm_key, osm_value) %>%
    osmdata_sf()
  pois_frompolygons = sf::st_centroid(q$osm_polygons)
  #mapview (pois_frompolygons)
  
  # Merge points and centroids, keeping all variables
  pois <- st_as_sf(data.table::rbindlist(list(pois_frompoints, pois_frompolygons), fill = TRUE))
  #mapview(pois)
  
  # Save as GeoJSON file
  directory = getCurrentFileLocation()
  outputfile = paste(directory, '/', output_file, sep = "");
  geojson_write(pois, file = outputfile)
}


# Define geographical area
area = "Cambridge, UK"

createPoisDataset (area, "supermarkets.geojson", "shop", "supermarkets")
createPoisDataset (area, "schools.geojson", "amenity", "schools")

