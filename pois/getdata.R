# Define geographical area
area = "Cambridge, UK"


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

# Load Tidyverse library
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Load geojsonio library
if (!require("geojsonio")) install.packages("geojsonio")
library(geojsonio)




# Obtain supermarkets - those entered in OSM as points
q <- getbb(area) %>%
  opq (nodes_only = TRUE, timeout=25*100) %>%
  add_osm_feature("shop", "supermarket") %>%
  osmdata_sf()
supermarkets_points = q$osm_points
#mapview (supermarkets_points)

# Obtain supermarkets - those entered in OSM as polygons
q <- getbb(area) %>%
  opq (timeout=25*100) %>%
  add_osm_feature("shop", "supermarket") %>%
  osmdata_sf()
supermarkets_polygons = sf::st_centroid(q$osm_polygons)
#mapview (supermarkets_polygons)

# Merge points and centroids, keeping all variables
supermarkets <- st_as_sf(data.table::rbindlist(list(supermarkets_points, supermarkets_polygons), fill = TRUE))
mapview(supermarkets)

# Define a function to obtain the directory of the script; see: https://stackoverflow.com/a/55322344
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

# Save as GeoJSON file
directory = getCurrentFileLocation()
outputfile = paste(directory, '/', 'supermarkets.geojson', sep = "");
geojson_write(supermarkets, file = outputfile)
