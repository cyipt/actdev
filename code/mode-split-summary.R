# Mode split summary by distance ------------------------------------------	
library(tidyverse)
# The Go Dutch scenario does not work for routes with a distance > 30km	
# mapview(desire_lines_scenario %>% filter(length >+ 30000))	

site_name = "chapelford" 
file = file.path("data-small", site_name, "all-census-od.csv")
desire_lines = read_csv(file = file)

sum_total = sum(desire_lines$all)	

mode_split = desire_lines %>% 	
  # st_drop_geometry() %>%	
  select(length, all, foot, bicycle, car_driver, pwalk_base:pdrive_base
         # , pwalk_godutch:pdrive_godutch
         ) %>% 	
  mutate(length_cat = case_when(	
    length < 1000 ~ "a.0-1",	
    length < 3000 ~ "b.1-3",	
    length < 6000 ~ "c.3-6",	
    length < 10000 ~ "d.6-10",	
    length < 15000 ~ "e.10-15",	
    length < 20000 ~ "f.15-20",	
    length < 30000 ~ "g.20-30",	
    length >= 30000 ~ "h.30+",	
  )) %>% 	
  group_by(length_cat) %>% 	
  summarise(
    all_base = sum(all),
    total_proportional = sum(all)/ sum_total,
    walk_base = sum(foot),
    cycle_base = sum(bicycle),
    drive_base = sum(car_driver),
    pwalk_base = weighted.mean(pwalk_base, w = all),	
    pcycle_base = weighted.mean(pcycle_base, w = all),	
    pdrive_base = weighted.mean(pdrive_base, w = all),	
    # , pwalk_godutch = weighted.mean(pwalk_godutch, w = all),	
    # pcycle_godutch = weighted.mean(pcycle_godutch, w = all),	
    # pdrive_godutch = weighted.mean(pdrive_godutch, w = all)
  ) %>%
  mutate(
    across(where(is.numeric), round, 2),
    across(c(all_base, walk_base:drive_base), round, 0)
    # , pwalk_godutch = ifelse(
    #   length_cat == "h.30+", NA, pwalk_godutch),
    # pcycle_godutch = ifelse(
    #   length_cat == "h.30+", NA, pcycle_godutch),
    # pdrive_godutch = ifelse(
    #   length_cat == "h.30+", NA, pdrive_godutch),
    # pwalk_godutch = ifelse(
    #   length_cat == "g.20-30", NA, pwalk_godutch),
    # pcycle_godutch = ifelse(
    #   length_cat == "g.20-30", NA, pcycle_godutch),
    # pdrive_godutch = ifelse(
    #   length_cat == "g.20-30", NA, pdrive_godutch)
    )

dsn = file.path("data-small", site_name, "mode-split.csv")	
file.remove(dsn)
readr::write_csv(mode_split, file = dsn)	
