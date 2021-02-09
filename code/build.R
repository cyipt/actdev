# Aim: create geojson data for ui for all sites

library(tidyverse)
library(sf)
library(stplanr)


# setwd("~/cyipt/actdev") # run this script from the actdev folder

smart.round = function(x) {
  y = floor(x)
  indices = utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y
}

# generic input data --------------------------------------------------------------
centroids_msoa = pct::get_centroids_ew() 
centroids_msoa = sf::st_transform(centroids_msoa, 4326)
zones_msoa_national = pct::get_pct(national = TRUE, geography = "msoa", layer = "z")
sf::st_crs(zones_msoa_national)
st_precision(zones_msoa_national) = 1000000

if(file.exists("od.Rds")) 
  od = readRDS("od.Rds") else
    od = pct::get_od()
# saveRDS(od, "od.Rds")

u = "https://github.com/cyipt/actdev/releases/download/0.1.1/all-sites.geojson"
sites = sf::st_read(u)
st_precision(sites) = 1000000

# 2011 MSOA populations - should it be a later year?
u2 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fmiddlesuperoutputareamidyearpopulationestimates%2fmid2011/mid2011msoaunformattedfile.xls"
f = "data/mid2011msoaunformattedfile.xls"
if(!file.exists(f)) {
  download.file(u2, f)
}
msoa_pops = readxl::read_xls(path = "data/mid2011msoaunformattedfile.xls", sheet = "Mid-2011 Persons", )
msoa_pops = msoa_pops %>% 
  select(geo_code1 = Code, msoa_population = "All Ages")

# estimated site populations
site_pops = sites %>% 
  st_drop_geometry() %>% 
  mutate(site_population = dwellings_when_complete * household_size)

# town centres
# piggyback::pb_download("English_Town_Centres_2004.zip", tag = "0.1.1")
# unzip("English_Town_Centres_2004.zip", exdir = "data")
# town_centres = st_read("data/English_Town_Centres_2004.shp")
# st_transform(town_centres, 4326)
# st_precision(town_centres) = 1000000
# town_centroids = town_centres %>% 
#   sf::st_as_sf(coords = c("CENTROIDX", "CENTROIDY"), crs = 27700) %>% 
#   st_transform(4326)
# sf::write_sf(town_centres, "town_centres.geojson")
# piggyback::pb_upload("town_centres.geojson")

# sf::write_sf(town_centroids, "town_centroids.geojson")
# piggyback::pb_upload("town_centroids.geojson")
town_centroids = sf::read_sf("town_centroids.geojson")

# jts data - SLOW
all_jts_tables = paste0("jts050", 1:8)
i = all_jts_tables[1]
for(i in all_jts_tables){
  year = 2017
  f = paste0(i, "-", year, ".geojson")
  if(! file.exists(f)) piggyback::pb_download(f, tag = "0.1.2")
  f2 = sf::read_sf(f)
  assign(i, f2)
  rm(f2)
}

# build for all site names
set.seed(2021)
site_names_to_builb = c(
  # sites %>% 
  #  sample_n(size = 5) %>%
  #  pull(site_name),
  c("taunton-firepool", "allerton-bywater", "handforth"),
  sites_extra = sites %>% 
    filter(str_detect(string = site_name, pattern = "chap|knei|bail")) %>% 
    pull(site_name)
)


data_dir = "data-small" # for test sites
dir.create(data_dir)
# note: fails for kidbrooke-village and long-marston
for(site_name in site_names_to_builb) {
  source("code/scenarios-streamlined.R")
}

zip(zipfile = "data-sites-2021-02-08.zip", files = "data-sites")
