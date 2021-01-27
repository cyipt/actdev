# Mode split summary by distance ------------------------------------------	

# The Go Dutch scenario does not work for routes with a distance > 30km	
# mapview(desire_lines_scenario %>% filter(length >+ 30000))	

sum_total = sum(desire_lines_scenario$all_commute_base)	

mode_split = desire_lines_scenario %>% 	
  st_drop_geometry() %>%	
  select(length, all_commute_base, pwalk_commute_base:pdrive_commute_base, pwalk_commute_godutch:pdrive_commute_godutch) %>% 	
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
  summarise(pwalk_commute_base = weighted.mean(pwalk_commute_base, w = all_commute_base),	
            pcycle_commute_base = weighted.mean(pcycle_commute_base, w = all_commute_base),	
            pdrive_commute_base = weighted.mean(pdrive_commute_base, w = all_commute_base),	
            pwalk_commute_godutch = weighted.mean(pwalk_commute_godutch, w = all_commute_base),	
            pcycle_commute_godutch = weighted.mean(pcycle_commute_godutch, w = all_commute_base),	
            pdrive_commute_godutch = weighted.mean(pdrive_commute_godutch, w = all_commute_base),	
            total = sum(all_commute_base)/ sum_total	
  ) %>% 	
  mutate(across(where(is.numeric), round, 2),	
         pwalk_commute_godutch = ifelse(	
           length_cat == "h.30+", NA, pwalk_commute_godutch),	
         pcycle_commute_godutch = ifelse(	
           length_cat == "h.30+", NA, pcycle_commute_godutch),	
         pdrive_commute_godutch = ifelse(	
           length_cat == "h.30+", NA, pdrive_commute_godutch),	
         pwalk_commute_godutch = ifelse(	
           length_cat == "g.20-30", NA, pwalk_commute_godutch),	
         pcycle_commute_godutch = ifelse(	
           length_cat == "g.20-30", NA, pcycle_commute_godutch),	
         pdrive_commute_godutch = ifelse(	
           length_cat == "g.20-30", NA, pdrive_commute_godutch))	

dsn = file.path("data-small", site_name, "mode-split.csv")	
readr::write_csv(mode_split, file = dsn)	
