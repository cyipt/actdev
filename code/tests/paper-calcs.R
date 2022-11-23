
# within site circuity ----------------------------------------------------


sites_join = sf::read_sf("data-small/all-sites.geojson")

circ_stats = sites_join %>% 
  filter(! is.na(in_site_walk_circuity),
         site_name != "micklefield") %>% 
  mutate(across(c(in_site_walk_circuity,in_site_cycle_circuity,in_site_drive_circuity), as.numeric))

mean(circ_stats$in_site_walk_circuity)
mean(circ_stats$in_site_cycle_circuity)
mean(circ_stats$in_site_drive_circuity)

circ_stats = circ_stats %>% 
  mutate(walk_drive_ratio = in_site_walk_circuity/in_site_drive_circuity,
         cycle_drive_ratio = in_site_cycle_circuity/in_site_drive_circuity)

mean(circ_stats$walk_drive_ratio)
mean(circ_stats$cycle_drive_ratio)

# mostly complete
incomplete = c("partly", "partly (partly before 2011)")
circ_complete = sites_join %>% 
  filter(! is.na(in_site_walk_circuity),
         ! is_complete %in% incomplete) %>% 
  mutate(across(c(in_site_walk_circuity,in_site_cycle_circuity,in_site_drive_circuity), as.numeric))

mean(circ_complete$in_site_walk_circuity)
mean(circ_complete$in_site_cycle_circuity)
mean(circ_complete$in_site_drive_circuity)

circ_complete = circ_complete %>% 
  mutate(walk_drive_ratio = in_site_walk_circuity/in_site_drive_circuity,
         cycle_drive_ratio = in_site_cycle_circuity/in_site_drive_circuity)

mean(circ_complete$walk_drive_ratio)
mean(circ_complete$cycle_drive_ratio)

summary(sites_join$crossing_points)


# old planit figure 2 ----------------------------------------------------------------


library(tidyverse)
beforechanges = read_csv("data/beforechanges.csv")
afterchanges = read_csv("data/afterchanges.csv")

withdwellings = afterchanges %>% 
  filter(! is.na(n_dwellings)) %>% 
  mutate(n_dwellings = as.integer(n_dwellings))

beforechanges$app_size = case_when(
  is.na(beforechanges$app_size) ~ "Other",
  beforechanges$app_size == "Small" ~ "Other",
  TRUE ~ beforechanges$app_size
)

afterchanges$app_size = case_when(
  is.na(afterchanges$app_size) ~ "Other",
  afterchanges$app_size == "Small" ~ "Other",
  TRUE ~ afterchanges$app_size
)

bc = beforechanges %>% 
  group_by(app_size) %>% 
  summarise(n = n()) %>% 
  mutate(status = "Before")

ac = afterchanges %>% 
  group_by(app_size) %>% 
  summarise(n = n()) %>% 
  mutate(status = "After")

ad = rbind(bc, ac)
ad$status = factor(ad$status, levels = c("Before", "After"))

ggplot(ad, aes(fill = status, y = n, x = app_size)) + 
  geom_bar(position = "dodge", stat="identity") +
  scale_fill_manual(values = c("#90be6d", "#457b9d")) +
  labs(x = "App size", y = "", fill = "") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size = 12))

# "#457b9d", "#90be6d", "#ffd166", "#fe5f55"

isna = afterchanges %>% 
  filter(is.na(app_size))

unique(isna$n_documents)
unique(isna$n_dwellings)
unique(isna$n_statutory_days)

# ----old planit figure 1------

withdwellings$dwellings_band = cut(x = withdwellings$n_dwellings, breaks = seq(from = 0, to = 400, by = 20), include.lowest = TRUE)
# change labels of withdwellings$dwellings_band

withdwellings$dwellings_band = gsub("\\(", "", withdwellings$dwellings_band)
withdwellings$dwellings_band = gsub(",","-", withdwellings$dwellings_band)
withdwellings$dwellings_band = gsub("\\]", "", withdwellings$dwellings_band)
withdwellings$dwellings_band = gsub("\\[", "", withdwellings$dwellings_band)
withdwellings$dwellings_band = as.character(withdwellings$dwellings_band)

withdwellings = withdwellings %>% 
  mutate_all(~replace(., is.na(.), 0))

withdwellings$dwellings_band = as.factor(withdwellings$dwellings_band)

withdwellings$dwellings_band = ordered(withdwellings$dwellings_band, levels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220", "220-240", "240-260", "260-280", "280-300", "300-320", "320-340", "340-360", "360-380", "380-400", ">400"))

ggplot(withdwellings, aes(x = dwellings_band)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45))


# Figure 4 (left half) ------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(sf)

site_name = "great-kneighton"

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

all_od_new = all_od %>% 
  rename(all = all, walk = foot, cycle = bicycle, drive = car_driver, trimode = trimode_base) %>% 
  select(geo_code2, all, trimode, walk, cycle, drive, length) %>% 
  mutate(across(all:length, as.numeric))

# disaggregated desire lines inside the study area
desire_lines_disag = desire_lines %>% 
  rename(all = all_base, trimode = trimode_base) %>% 
  mutate(distance_band = get_distance_bands(x = length)) %>% 
  group_by(distance_band, .drop = FALSE) %>% 
  filter(purpose == "commute") %>% 
  select(geo_code2, all, trimode, walk_base, cycle_base, drive_base, walk_godutch, cycle_godutch, drive_godutch, length) %>% 
  sf::st_drop_geometry()

# get desire lines outside the study area
all_dist_outside = all_od_new %>% 
  filter(! geo_code2 %in% desire_lines_disag$geo_code2)

# join the lines inside and outside the study area for the two scenarios separately
desire_disag_base = desire_lines_disag %>%
  select(-c(walk_godutch:drive_godutch)) %>% 
  rename(walk = walk_base, cycle = cycle_base, drive = drive_base)

desire_disag_scenario = desire_lines_disag %>%
  select(-c(walk_base:drive_base)) %>% 
  rename(walk = walk_godutch, cycle = cycle_godutch, drive = drive_godutch)

desire_all_base = bind_rows(desire_disag_base, all_dist_outside)

desire_all_scenario = bind_rows(desire_disag_scenario, all_dist_outside)

# Baseline scenario

mode_split_base = desire_all_base %>%
  mutate(distance_band = get_distance_bands(x = length)) %>% 
  group_by(distance_band, .drop = FALSE) %>% 
  summarise(across(all:drive, sum)) %>% 
  mutate(other = all - trimode)

all_dist = mode_split_base %>% 
  pivot_longer(cols = c(walk, cycle, drive, other))

all_dist$name = factor(all_dist$name, levels = c("walk", "cycle", "other", "drive"))

g1 = ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity"
           # , show.legend = FALSE # this makes the bars too wide
           ) +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal() +
  ylim(c(0, 800))
ggsave(filename = "great-kneighton-baseline.png", plot = g1, dpi = 300)
# g1 + theme(text = element_text(size = 20)) 


# Figure 4 (right half) ---------------------------------------------------

site_name = "chapelford"

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

all_od_new = all_od %>% 
  rename(all = all, walk = foot, cycle = bicycle, drive = car_driver, trimode = trimode_base) %>% 
  select(geo_code2, all, trimode, walk, cycle, drive, length) %>% 
  mutate(across(all:length, as.numeric))

# disaggregated desire lines inside the study area
desire_lines_disag = desire_lines %>% 
  rename(all = all_base, trimode = trimode_base) %>% 
  mutate(distance_band = get_distance_bands(x = length)) %>% 
  group_by(distance_band, .drop = FALSE) %>% 
  filter(purpose == "commute") %>% 
  select(geo_code2, all, trimode, walk_base, cycle_base, drive_base, walk_godutch, cycle_godutch, drive_godutch, length) %>% 
  sf::st_drop_geometry()

# get desire lines outside the study area
all_dist_outside = all_od_new %>% 
  filter(! geo_code2 %in% desire_lines_disag$geo_code2)

# join the lines inside and outside the study area for the two scenarios separately
desire_disag_base = desire_lines_disag %>%
  select(-c(walk_godutch:drive_godutch)) %>% 
  rename(walk = walk_base, cycle = cycle_base, drive = drive_base)

desire_disag_scenario = desire_lines_disag %>%
  select(-c(walk_base:drive_base)) %>% 
  rename(walk = walk_godutch, cycle = cycle_godutch, drive = drive_godutch)

desire_all_base = bind_rows(desire_disag_base, all_dist_outside)

desire_all_scenario = bind_rows(desire_disag_scenario, all_dist_outside)

# Baseline scenario

mode_split_base = desire_all_base %>%
  mutate(distance_band = get_distance_bands(x = length)) %>% 
  group_by(distance_band, .drop = FALSE) %>% 
  summarise(across(all:drive, sum)) %>% 
  mutate(other = all - trimode)

all_dist = mode_split_base %>% 
  pivot_longer(cols = c(walk, cycle, drive, other))

all_dist$name = factor(all_dist$name, levels = c("walk", "cycle", "other", "drive"))

g2 = ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal() +
  ylim(c(0, 800))
ggsave(filename = "chapelford-baseline.png", plot = g2, dpi = 300)
# g2 + theme(text = element_text(size = 20))



# Figure 5 ----------------------------------------------------------------

site_name = "dickens-heath"

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

all_od_new = all_od %>%
  rename(all = all, walk = foot, cycle = bicycle, drive = car_driver, trimode = trimode_base) %>%
  select(geo_code2, all, trimode, walk, cycle, drive, length) %>%
  mutate(across(all:length, as.numeric))

# disaggregated desire lines inside the study area
desire_lines_disag = desire_lines %>%
  rename(all = all_base, trimode = trimode_base) %>%
  mutate(distance_band = get_distance_bands(x = length)) %>%
  group_by(distance_band, .drop = FALSE) %>%
  filter(purpose == "commute") %>%
  select(geo_code2, all, trimode, walk_base, cycle_base, drive_base, walk_godutch, cycle_godutch, drive_godutch, length) %>%
  sf::st_drop_geometry()

# get desire lines outside the study area
all_dist_outside = all_od_new %>%
  filter(! geo_code2 %in% desire_lines_disag$geo_code2)

# join the lines inside and outside the study area for the two scenarios separately
desire_disag_base = desire_lines_disag %>%
  select(-c(walk_godutch:drive_godutch)) %>%
  rename(walk = walk_base, cycle = cycle_base, drive = drive_base)

desire_disag_scenario = desire_lines_disag %>%
  select(-c(walk_base:drive_base)) %>%
  rename(walk = walk_godutch, cycle = cycle_godutch, drive = drive_godutch)

desire_all_base = bind_rows(desire_disag_base, all_dist_outside)

desire_all_scenario = bind_rows(desire_disag_scenario, all_dist_outside)

# Baseline scenario

mode_split_base = desire_all_base %>%
  mutate(distance_band = get_distance_bands(x = length)) %>%
  group_by(distance_band, .drop = FALSE) %>%
  summarise(across(all:drive, sum)) %>%
  mutate(other = all - trimode)

all_dist = mode_split_base %>%
  pivot_longer(cols = c(walk, cycle, drive, other))

all_dist$name = factor(all_dist$name, levels = c("walk", "cycle", "other", "drive"))

all_dist = all_dist %>% 
  mutate(scenario = "baseline")

# Go Active scenario

mode_split_active = desire_all_scenario %>%
  mutate(distance_band = get_distance_bands(x = length)) %>%
  group_by(distance_band, .drop = FALSE) %>%
  summarise(across(all:drive, sum)) %>%
  mutate(other = all - trimode)

all_dist_active = mode_split_active %>%
  pivot_longer(cols = c(walk, cycle, drive, other))

all_dist_active$name = factor(all_dist$name, levels = c("walk", "cycle", "other", "drive"))

all_dist_active = all_dist_active %>% 
  mutate(scenario = "active")

all_dist_both = rbind(all_dist, all_dist_active)
all_dist_both$scenario = factor(all_dist_both$scenario, levels = c("baseline", "active"))

g3 = ggplot(all_dist_both, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity"
           # , show.legend = FALSE # this makes the bars too wide
  ) +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  facet_wrap(~ scenario) +
  theme_minimal() +
  theme(strip.text.x = element_blank()) +
  ylim(c(0, 500))

ggsave(filename = "dickens-heath.png", plot = g3, dpi = 300)
# g3 + theme(text = element_text(size = 20))
