# Aim: create geojson data for ui for all sites

library(tidyverse)
library(sf)
library(stplanr)

set.seed(2021)
site_names_to_build = c(
  # sites %>% 
  #  sample_n(size = 5) %>%
  #  pull(site_name),
  c("taunton-firepool", "allerton-bywater", "handforth"),
  sites_extra = sites %>% 
    filter(str_detect(string = site_name, pattern = "chap|knei|bail")) %>% 
    pull(site_name)
)

site_names_to_build = "great-kneighton"
# site_names_to_build = "kidbrooke-village"
data_dir = "data-small" # for test sites
# dir.create(data_dir)
# note: fails for kidbrooke-village and long-marston
for(site_name in site_names_to_build) {
  source("code/scenarios-streamlined.R")
}

# zip(zipfile = "data-sites-2021-02-08.zip", files = "data-sites")

# Add json files for abstreet
site_directories = list.dirs(data_dir)[-1]
site_names = gsub(pattern = "data-small/", replacement = "", x = site_directories)
i = 1
for(i in seq(length(site_directories))) {
  site_name = site_names[i]
  source("code/abstr-scenarios.R")
}
