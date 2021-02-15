# Aim: create geojson data for ui for all sites

library(tidyverse)
library(sf)
library(stplanr)
max_length = 20000 # maximum length of desire lines in m
household_size = 2.3 # mean UK household size at 2011 census
min_flow_routes = 10 # threshold above which OD pairs are included
region_buffer_dist = 2000
large_area_buffer = 500
sites = sf::read_sf("data-small/all-sites.geojson")
source("code/build-setup.R") # national data

# build aggregate scenarios ----------------------------------------------
# site_names_to_build = "kidbrooke-village"
set.seed(2021) # reproducibility
site_names_to_build = sites %>% 
  # fails on the abstreet scenarios for some reason...
  filter(!str_detect(string = site_name, pattern = "bath|ebb|handf|tyersal-lane")) %>%
  # slice(1:5) %>%
  pull(site_name)
data_dir = "data-small" # for test sites
# dir.create(data_dir)
# note: fails for kidbrooke-village and long-marston
# site_names_to_build = "allerton-bywater"
for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/scenarios-streamlined.R")
    })
  })
}

# Add json files for abstreet ---------------------------------------------
# site_directories = list.dirs(data_dir)[-1]
# site_names_to_build = gsub(pattern = "data-small/", replacement = "", x = site_directories)
for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/abstr-scenarios.R")
    })
  })
}

# Generate 'clockboard' data ----------------------------------------------

source("code/tests/color_palette.R")

for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/clockboard-zones.R")
    })
  })
}

# Generate infographics  ----------------------------------------------

for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/infographics.R")
    })
  })
}

# Generate mode split summary  ----------------------------------------------

for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/mode-split-summary.R")
    })
  })
}

# zip(zipfile = "data-sites-2021-02-08.zip", files = "data-small")