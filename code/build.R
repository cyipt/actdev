# Aim: create geojson data for ui for all sites

library(tidyverse)
library(sf)
library(stplanr)
max_length = 20000 # maximum length of desire lines in m
household_size = 2.3 # mean UK household size at 2011 census
min_flow_routes = 2 # threshold above which OD pairs are included
region_buffer_dist = 2000
large_area_buffer = 500
new_site = TRUE
data_dir = "data-small" # for test sites

# If new site has been added use the rbind version of sites
if(!exists("sites")){
  sites = sf::read_sf("data-small/all-sites.geojson")
}

if(new_site) {
  # read-in new site that must have the following fields (NAs allowed):
  # dwellings_when_complete, site_name and full_name are necessary
  # [1] "site_name"               "full_name"               "main_local_authority"   
  # [4] "is_complete"             "dwellings_when_complete" "planning_url"           
  # [7] "geometry"  
  site = sf::read_sf("map.geojson")
  sf::st_crs(site) = 4326
  site_names_to_build = site$site_name
  path = file.path(data_dir, site_names_to_build)
  dir.create(path)
  new_cols = sf::st_drop_geometry(sites[1, ])
  new_cols = new_cols[setdiff(names(sites), names(site))]
  new_cols[] = NA
  sites = rbind(
    sites,
    sf::st_sf(
      cbind(sf::st_drop_geometry(site), new_cols),
      geometry = site$geometry
      )
  )
} else {
  site_names_to_build = sites %>% 
    filter(str_detect(string = site_name, pattern = "regex-to-rebuild"))
}

source("code/load_jts.R") # national data if not loaded (takes some time)
source("code/build-setup.R") # national data

# build aggregate scenarios ----------------------------------------------
set.seed(2021) # reproducibility
disaggregate_desire_lines = FALSE

for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/scenarios-streamlined.R")
    })
  })
}
# check the output by uncommenting this line:
# mapview::mapview(desire_lines_many)

# Add jts data ------------------------------------------------------------
for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  suppressMessages({
    suppressWarnings({
      source("code/add_jts.R")
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

# Populate site metrics for new site --------------------------------------

if(new_site){
  suppressMessages({
    suppressWarnings({
      source("code/site-metrics.R")
    })
  })
}
file.remove("data-small/all-sites.geojson")
file.remove("all-sites.geojson")

# Save and sanity check data -----------------------------------------------

write_csv(sites_join, "data-small/all-sites.csv")
sf::write_sf(sites_join,"data-small/all-sites.geojson")

mode_share_baseline = read.csv("data-small/mode-share-sites-baseline.csv")
mode_share_goactive = read.csv("data-small/mode-share-sites-goactive.csv")
summary(sanity1 <- mode_share_baseline$site_name == sites_join$site_name)
summary(sanity2 <- mode_share_goactive$site_name == sites_join$site_name)
mode_share_goactive$site_name[!sanity2]
setdiff(mode_share_goactive$site_name, sites_join$site_name)
all(sanity1)
all(sanity2)

# Then in Git do git add -A, commit and push
# git add -A
# git commit -am 'Update sites'
# git push
