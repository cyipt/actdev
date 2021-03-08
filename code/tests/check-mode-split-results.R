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





