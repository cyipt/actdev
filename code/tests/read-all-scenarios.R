# Aim: read-in national data for descriptive analysis and sanity checking

library(tidyverse)
dirs = list.dirs("data-small", full.names = TRUE, recursive = FALSE)[-1]
d = dirs[1]
d
read_desire_lines = function(d) {
  f = file.path(d, "desire-lines-few.geojson")
  site = gsub(pattern = "data-small/|/desire-lines-few.geojson", replacement = "", x = f)
  s = sf::read_sf(f)
  s$site = site
  s
}
desire_lines_few = map_dfr(dirs, read_desire_lines)
summary(desire_lines_few)

desire_lines_few %>% 
  filter(drive_godutch < 0) %>% 
  select(site)

get_scenario_size = function(d) {
  file.size(file.path(d, "scenario_go_active.json")) /
  file.size(file.path(d, "scenario_base.json"))
}
map_dbl(dirs, get_scenario_size)
