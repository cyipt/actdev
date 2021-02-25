# Mode split summary by distance ------------------------------------------	

library(tidyverse)
library(ggplot2)
library(sf)

if(!exists("site_name")) site_name = "great-kneighton"

path = file.path("data-small", site_name)
all_od = read_csv(file.path(path, "all-census-od.csv"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-many.geojson"))

# colourscheme
cols = c("#457b9d", "#90be6d", "#ffd166", "#fe5f55")

# create combined infographic ---------------------------------------------

get_distance_bands = function(x, distance_band = c(0, zonebuilder::zb_100_triangular_numbers[2:5], 20, 30, 10000)) {
  distance_labels = paste0(distance_band[-length(distance_band)], "-", distance_band[-1])
  distance_labels = gsub(pattern = "-10000", replacement = "+", distance_labels)
  cut(x = x, breaks = distance_band * 1000, labels = distance_labels)
}

# Baseline scenario
all_od_new = all_od %>% 
  rename(all = all, walk = foot, cycle = bicycle, drive = car_driver, trimode = trimode_base) %>% 
  select(geo_code2, all, trimode, walk, cycle, drive, length) %>% 
  mutate(across(all:length, as.numeric))

mode_split_base = all_od_new %>%
  mutate(distance_band = get_distance_bands(x = length)) %>% 
  group_by(distance_band, .drop = FALSE) %>% 
  summarise(across(all:drive, sum)) %>% 
  mutate(other = all - trimode)

all_dist = mode_split_base %>% 
  pivot_longer(cols = c(walk, cycle, drive, other))

all_dist$name = factor(all_dist$name, levels = c("walk", "cycle", "other", "drive"))
g1 = ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()

# Go Active scenario
names(desire_lines) = gsub(pattern = "_godutch", replacement = "", x = names(desire_lines))
summary(desire_lines$length)

desire_lines_scenario = desire_lines %>% 
  mutate(all = all_base, trimode = trimode_base) %>% 
  mutate(distance_band = get_distance_bands(x = length)) %>% 
  group_by(distance_band, .drop = FALSE) %>% 
  filter(purpose == "commute") %>% 
  select(geo_code2, all, trimode, walk, cycle, drive, length) %>% 
  sf::st_drop_geometry()

# join on actdev scenario data
all_dist_outside = all_od_new %>% 
  filter(! geo_code2 %in% desire_lines$geo_code2)

desire_plus_all = bind_rows(desire_lines_scenario, all_dist_outside)

mode_split_scenario = desire_plus_all %>%
  # sf::st_drop_geometry() %>%
  mutate(distance_band = get_distance_bands(x = length)) %>% 
  group_by(distance_band, .drop = FALSE) %>% 
  summarise(across(all:drive, sum)) %>% 
  mutate(other = all - trimode)

all_dist_scenario = mode_split_scenario %>% 
  pivot_longer(cols = c(walk, cycle, drive, other))

all_dist_scenario$name = factor(all_dist_scenario$name, levels = c("walk", "cycle", "other", "drive"))

g2 = ggplot(all_dist_scenario, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()

# library(patchwork)
# infographic = g1 + g2

dsn = file.path("data-small", site_name, "mode-split-base.png")
ggsave(filename = dsn, width = 4, height = 3, dpi = 100, plot = g1)
# magick::image_read(dsn) # sanity check, looking good!

dsn = file.path("data-small", site_name, "mode-split-goactive.png")
ggsave(filename = dsn, width = 4, height = 3, dpi = 100, plot = g2)

# Create single mode split summary csv
sum_total = sum(mode_split_base$all)

mode_split_all = mode_split_base %>% 
  mutate(
    proportion_in_distance_band = round(100 * all / sum_total),
    walk_goactive = mode_split_scenario$walk,
    cycle_goactive = mode_split_scenario$cycle,
    drive_goactive = mode_split_scenario$drive,
    other_goactive = mode_split_scenario$other,
    percent_walk_base = round(100 * walk / all),	
    percent_cycle_base = round(100 * cycle / all),	
    percent_drive_base = round(100 * drive / all),
    percent_other_base = round(100 * other / all),
    percent_walk_goactive = round(100 * walk_goactive / all),	
    percent_cycle_goactive = round(100 * cycle_goactive / all),	
    percent_drive_goactive = round(100 * drive_goactive / all),
    percent_other_goactive = round(100 * other_goactive / all)
  ) %>% 
  rename(
    walk_base = walk,
    cycle_base = cycle,
    drive_base = drive,
    other_base = other
  ) %>% 
  select(distance_band, proportion_in_distance_band, everything())

dsn = file.path("data-small", site_name, "mode-split.csv")	
file.remove(dsn)
readr::write_csv(mode_split_all, file = dsn)	
