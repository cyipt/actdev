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
  filter(str_detect(string = site_name, pattern = "kneighton|allert|pound")) %>% 
  pull(site_name)

data_dir = "data-small" # for test sites
disaggregate_desire_lines = TRUE
for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/scenarios-streamlined.R")
    })
  })
}

# Add json files for abstreet ---------------------------------------------
# should the build process add a background traffic scenario? (WIP)
build_background_traffic = FALSE
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

# Generate in site metrics  ----------------------------------------------

for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/in-site-metrics.R")
    })
  })
}

# Update site.geojson files -----------------------------------------------

all_sites = sf::read_sf("data-small/all-sites.geojson")
for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  f = file.path("data-small", site_name, "site.geojson")
  site_original = sf::read_sf(f)
  (n1 = names(site_original))
  (n2 = names(all_sites))
  setdiff(n2, n1)
  (in_original_not_all = n1[!n1 %in% n2])
  (extra_vars = n2[!n2 %in% n1])
  site_original_not_in_all = site_original[c("site_name", in_original_not_all)]
  site_new = left_join(site_original_not_in_all, all_sites %>% sf::st_drop_geometry()) %>% 
    mutate_if(is.numeric, function(x) round(x, digits = 2))
  file.remove(f)
  sf::write_sf(site_new, f)
}
all_sites %>% filter(site_name == "tresham") %>% 
  select(percent_commute_active_base:percent_drive_convertable)

# zip(zipfile = "data-sites-2021-02-08.zip", files = "data-small")