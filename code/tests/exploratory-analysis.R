
library(ggplot2)
library(sf)
library(tidyverse)

site_name = "great-kneighton"   # which site to look at (can change)
data_dir = "data-small" # for test sites

path = file.path("data-small", site_name)

dart = read_sf(file.path(path, "dartboard.geojson"))
routes_fast = read_sf(file.path(path, "routes-fast.geojson"))
site = read_sf(file.path(path, "site.geojson"))
mode_split = read_csv(file.path(path, "mode-split.csv"))
all_od = read_csv(file.path(path, "all-census-od.csv"))
desire_lines = sf::read_sf(file.path(path, "desire-lines-many.geojson"))

mapview::mapview(dart["circuity_walk"]) +
  mapview(routes_fast["all_base"])

summary(all_od)
mean(all_od$length)
max(all_od$length)
# weighted.mean(all_od$length, w = all_od$all)

sum(all_od$all)
dim(all_od)

sum(routes_fast$all_base)

# create stacked bar chart of baseline commute mode / distance from `mode_split`
# with a bar for each distance category, containing stacked travel modes
# walking/cycling in high distance bands is likely to be an unrealistic side effect of using data from large MSOAs  

mode_split$other_base = mode_split$all_base - mode_split$walk_base - mode_split$cycle_base - mode_split$drive_base
mode_long = pivot_longer(mode_split, cols = c(walk_base, cycle_base, drive_base, other_base))
mode_long$name = factor(mode_long$name, levels = c("walk_base", "cycle_base", "other_base", "drive_base"))
mode_long$distance_band = mode_long$length_cat

ggplot(mode_long, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkblue", "blue", "purple", "red"))

# # comparing scenarios
# distance_band = c(0, zonebuilder::zb_100_triangular_numbers[1:5], 20, 30, 10000)
# distance_labels = paste0(distance_band[-length(distance_band)], "-", distance_band[-1])
# distance_labels = gsub(pattern = "-10000", replacement = "+", distance_labels)
# all_desire$distance_band = cut(x = all_desire$length, breaks = distance_band * 1000, labels = distance_labels)
# (shared_names = union(names(all_desire), names(all_od)))
# table(all_desire$distance_band)
# 
# mode_split_old = all_desire %>%
#   sf::st_drop_geometry() %>% 
#   group_by(distance_band, .drop = FALSE) %>% 
#   summarise(across(all_base:drive_base, sum)) %>% 
#   mutate(other_base = all_base - trimode_base)
# 
# mode_long_old = pivot_longer(mode_split_old, cols = c(walk_base, cycle_base, drive_base, other_base))
# unique(mode_long_old$name)
# # mode_long_old$name = factor(mode_long_old$name, factors)
# ggplot(mode_long_old, aes(fill = name, y = value, x = distance_band)) +
#   geom_bar(position = "stack", stat = "identity") +
#   scale_fill_manual(values = c("darkblue", "blue", "purple", "red"))
# 
# all_desire$drive_base = all_desire$drive_godutch
# all_desire$cycle_base = all_desire$cycle_godutch
# all_desire$walk_base = all_desire$walk_godutch
# 
# all_desire = all_desire %>% select(any_of(shared_names))
# 
# mode_split_new = all_desire %>%
#   sf::st_drop_geometry() %>% 
#   group_by(distance_band, .drop = FALSE) %>% 
#   summarise(across(all_base:drive_base, sum)) %>% 
#   mutate(other_base = all_base - trimode_base)
# 
# mode_long_new = pivot_longer(mode_split_new, cols = c(walk_base, cycle_base, drive_base, other_base))
# 
# ggplot(mode_long_new, aes(fill = name, y = value, x = distance_band)) +
#   geom_bar(position = "stack", stat = "identity") +
#   scale_fill_manual(values = c("darkblue", "blue", "purple", "red"))
# 
# 
# names(mode_split)
# 
# mode_split$other_base = mode_split$all_base - mode_split$walk_base - mode_split$cycle_base - mode_split$drive_base


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
  scale_fill_manual(values = c("darkblue", "blue", "purple", "red"))

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
  scale_fill_manual(values = c("darkblue", "blue", "purple", "red"))

g2

library(patchwork)
g1 + g2

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
