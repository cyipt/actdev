# Aim: create geojson data for ui for all sites

library(tidyverse)
library(sf)
library(stplanr)

set.seed(2021)
# site_names_to_build = c(
#   # sites %>% 
#   #  sample_n(size = 5) %>%
#   #  pull(site_name),
#   c("taunton-firepool", "allerton-bywater", "handforth"),
#   sites_extra = sites %>% 
#     filter(str_detect(string = site_name, pattern = "chap|knei|bail")) %>% 
#     pull(site_name)
# )

# site_names_to_build = "great-kneighton"
# site_names_to_build = "kidbrooke-village"
site_names_to_build = sites %>% 
  filter(!str_detect(string = site_name, pattern = "allerton-bywater")) %>% 
  slice(10:15) %>% 
  pull(site_name)
data_dir = "data-small" # for test sites
# dir.create(data_dir)
# note: fails for kidbrooke-village and long-marston
site_names_to_build = "allerton-bywater"
for(site_name in site_names_to_build) {
  source("code/scenarios-streamlined.R")
}

# zip(zipfile = "data-sites-2021-02-08.zip", files = "data-sites")

# Add json files for abstreet
# site_directories = list.dirs(data_dir)[-c(1:2)]
# site_names = gsub(pattern = "data-small/", replacement = "", x = site_directories)

i = 1
for(site_name in site_names_to_build) {
  source("code/abstr-scenarios.R")
}

