# Aim: check scenario results for https://github.com/cyipt/actdev/issues/129

library(tidyverse)

f = "https://github.com/cyipt/actdev/raw/main/data-small/allerton-bywater/desire-lines-many.geojson"
desire_lines_final = sf::read_sf(f)
desire_lines_final

base_mode_totals = desire_lines_final %>%
  sf::st_drop_geometry() %>%
  select(matches("base")) %>% 
  select(-matches("all|tri")) %>% 
  colSums()
dutch_mode_totals = desire_lines_final %>%
  sf::st_drop_geometry() %>%
  select(matches("dutch")) %>% 
  select(-matches("all|tri")) %>% 
  colSums()

base_mode_totals
dutch_mode_totals

(shift_totals = dutch_mode_totals - base_mode_totals)
if(sum(shift_totals) != 0) stop("Mode totals do not add up.")

min_vals = sapply(desire_lines_final %>% sf::st_drop_geometry() %>% select_if(is.numeric), min)
if(any(min_vals < 0)) stop("Negative values detected")

library(tidyverse)
f = "https://github.com/cyipt/actdev/raw/main/data-small/allerton-bywater/mode-split.csv"
desire_lines_final = readr::read_csv(f)
base_results = desire_lines_final %>% 
  select(distance_band, matches("base"), -matches("perc")) %>% 
  tidyr::pivot_longer(cols = matches("base"), names_to = "mode")
g1 = ggplot(base_results) +
  geom_bar(aes(distance_band, value, fill = mode), stat = "identity")

go_results = desire_lines_final %>% 
  select(distance_band, matches("go"), -matches("perc")) %>% 
  tidyr::pivot_longer(cols = matches("go"), names_to = "mode")
g2 = ggplot(go_results) +
  geom_bar(aes(distance_band, value, fill = mode), stat = "identity")

library(patchwork)
g1 + g2

desire_lines_final

base_mode_totals = desire_lines_final %>%
  sf::st_drop_geometry() %>%
  select(matches("base")) %>% 
  select(-matches("all|tri")) %>% 
  colSums()
dutch_mode_totals = desire_lines_final %>%
  sf::st_drop_geometry() %>%
  select(matches("dutch")) %>% 
  select(-matches("all|tri")) %>% 
  colSums()

base_mode_totals
dutch_mode_totals

(shift_totals = dutch_mode_totals - base_mode_totals)
if(sum(shift_totals) != 0) stop("Mode totals do not add up.")

min_vals = sapply(desire_lines_final %>% sf::st_drop_geometry() %>% select_if(is.numeric), min)
if(any(min_vals < 0)) stop("Negative values detected")


# Illustrate the uptake model

od_national = pct::get_od()
centroids_msoa = pct::get_centroids_ew()
set.seed(2021)
od_sample = od_national %>% 
  filter(geo_code1 %in% centroids_msoa$msoa11cd) %>% 
  filter(geo_code2 %in% centroids_msoa$msoa11cd) %>%
  filter(all > 10) %>% 
  sample_n(100000, weight = all)

sum(od_sample$all) / sum(od_national$all) # 40% trips

desire_lines_sample = od::od_to_sf(od_sample, centroids_msoa)
desire_lines_sample$distance = stplanr::geo_length(desire_lines_sample)
dist_breaks = c(seq(from = 0.2, to = 4.5, by = 0.2),  seq(from = 5, to = 21, by = 2)) * 1000

summary(desire_lines_sample$distance)

desire_lines_modesplit = desire_lines_sample %>%
  filter(distance < 20000) %>% 
  filter(distance > 300) %>% 
  mutate(
  distance_band_fine = cut(distance, dist_breaks),
  pwalk_base = foot / all,
  pwalk_godutch = case_when(	
    distance <= 2000 ~ pwalk_base + 0.3, # 30% shift walking for routes >2km
    distance <= 2500 ~ pwalk_base + 0.2, # 20% shift walking for routes >2.5km
    distance <= 3000 ~ pwalk_base + 0.1, # 10% shift walking for routes >3km
    distance <= 6000 ~ pwalk_base + 0.05, # 5% shift walking for routes 3-6km	
    TRUE ~ pwalk_base),
  walk_godutch = pwalk_godutch * all
) %>% 
  mutate(
    pcycle_godutch_uptake = pct::uptake_pct_godutch_2020(distance = distance, gradient = 0.01),
    cycle_godutch_additional = pcycle_godutch_uptake * car_driver,
    cycle_godutch = bicycle + cycle_godutch_additional,
    pcycle_godutch = cycle_godutch / all,
    drive_godutch = car_driver - cycle_godutch_additional # ensure totals add up
  )

table(desire_lines_modesplit$distance_band_fine)

desire_lines_summary = desire_lines_modesplit %>% 
  sf::st_drop_geometry() %>% 
  group_by(distance_band_fine) %>% 
  summarise(
    distance = mean(distance) / 1000,
    pwalk = sum(foot) / sum(all),
    pwalk_go_active = sum(walk_godutch) / sum(all),
    pcycle = sum(bicycle) / sum(all),
    pcycle_go_active = sum(cycle_godutch) / sum(all),
    pdrive = sum(car_driver) / sum(all),
    pdrive_go_active = sum(drive_godutch) / sum(all),
  ) %>% 
  select(matches("p|dist")) %>% 
  pivot_longer(cols = matches("p"), names_to = "ScenarioMode", values_to = "Proportion")

table(desire_lines_summary$ScenarioMode)

desire_lines_summary$Mode = NA
desire_lines_summary$Scenario = "Baseline"
desire_lines_summary$Mode[grepl(pattern = "cycle", x = desire_lines_summary$ScenarioMode)] = "Cycle"
desire_lines_summary$Mode[grepl(pattern = "foot|walk", x = desire_lines_summary$ScenarioMode)] = "Walk"
desire_lines_summary$Mode[grepl(pattern = "drive", x = desire_lines_summary$ScenarioMode)] = "Drive"
desire_lines_summary$Scenario[grepl(pattern = "active", x = desire_lines_summary$ScenarioMode)] = "Go Active"
desire_lines_summary$Mode = factor(desire_lines_summary$Mode, levels = c("Drive", "Cycle", "Walk"))

# first pass
desire_lines_summary %>% 
  ggplot() +
  # geom_line(aes(distance, value, colour = Mode)) %>% 
  geom_line(aes(distance, Proportion)) +
  facet_grid(Mode ~ Scenario) +
  scale_y_continuous(labels = scales::percent)

ggsave("data-small/scenario-overview.png")


desire_lines_summary = desire_lines_modesplit %>% 
  sf::st_drop_geometry() %>% 
  group_by(distance_band_fine) %>% 
  summarise(
    distance = mean(distance) / 1000,
    pwalk = sum(foot) / sum(all),
    pwalk_90th = quantile(foot / all, 0.8),
    pwalk_10th = quantile(foot / all, 0.2),
    pwalk_go_active = sum(walk_godutch) / sum(all),
    pwalk_go_active_90th = quantile(walk_godutch / all, 0.8),
    pwalk_go_active_10th = quantile(walk_godutch / all, 0.2),
    pcycle = sum(bicycle) / sum(all),
    pcycle_90th = quantile(bicycle / all, 0.8),
    pcycle_10th = quantile(bicycle / all, 0.2),
    pcycle_go_active = sum(cycle_godutch) / sum(all),
    pcycle_go_active_90th = quantile(cycle_godutch / all, 0.8),
    pcycle_go_active_10th = quantile(cycle_godutch / all, 0.2),
    pdrive = sum(car_driver) / sum(all),
    pdrive_90th = quantile(car_driver / all, 0.8),
    pdrive_10th = quantile(car_driver / all, 0.2),
    pdrive_go_active = sum(drive_godutch) / sum(all),
    pdrive_go_active_90th = quantile(drive_godutch / all, 0.8),
    pdrive_go_active_10th = quantile(drive_godutch / all, 0.2),
  ) %>% 
  select(matches("p|dist")) %>% 
  pivot_longer(cols = matches("p"), names_to = "ScenarioMode", values_to = "Proportion")

desire_lines_summary$Mode = NA
desire_lines_summary$Scenario = "Baseline"
desire_lines_summary$Mode[grepl(pattern = "cycle", x = desire_lines_summary$ScenarioMode)] = "Cycle"
desire_lines_summary$Mode[grepl(pattern = "foot|walk", x = desire_lines_summary$ScenarioMode)] = "Walk"
desire_lines_summary$Mode[grepl(pattern = "drive", x = desire_lines_summary$ScenarioMode)] = "Drive"
desire_lines_summary$Scenario[grepl(pattern = "active", x = desire_lines_summary$ScenarioMode)] = "Go Active"
desire_lines_summary$Mode = factor(desire_lines_summary$Mode, levels = c("Drive", "Cycle", "Walk"))

table(desire_lines_summary_all$ScenarioMode)
desire_lines_q90 = desire_lines_summary %>% filter(str_detect(string = ScenarioMode, pattern = "90th"))
desire_lines_q10 = desire_lines_summary %>% filter(str_detect(string = ScenarioMode, pattern = "10th"))
desire_lines_summary = desire_lines_summary %>% filter(!str_detect(string = ScenarioMode, pattern = "th"))
desire_lines_summary$Proportion_90th = desire_lines_q90$Proportion
desire_lines_summary$Proportion_10th = desire_lines_q10$Proportion

cols = c("#457b9d", "#90be6d", "#ffd166", "#fe5f55")
cols = rev(cols[-3])


desire_lines_summary %>% 
  ggplot() +
  # geom_line(aes(distance, value, colour = Mode)) %>% 
  geom_line(aes(distance, Proportion, colour = Mode)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(aes(distance, ymin = Proportion_10th, ymax = Proportion_90th, fill = Mode), alpha = 0.3) +
  scale_fill_manual(values = cols) +
  facet_grid(vars(Scenario)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(name = "Euclidean Distance (km)")

ggsave("data-small/scenario-overview.png")


