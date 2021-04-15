for(site_name in site_names_to_build) {
  message("Building for ", site_name)
  f = file.path("data-small", site_name, "site.geojson")
  site_original = sf::read_sf(f)
  (n1 = names(site_original))
  (n2 = names(all_sites))
  setdiff(n2, n1)
  (in_original_not_all = n1[!n1 %in% n2])
  (extra_vars = n2[!n2 %in% n1])
  site_original_not_in_all = site_original[c("site_name", in_original_not_all)]
  site_new = left_join(site_original_not_in_all, all_sites %>% sf::st_drop_geometry()) %>% 
    mutate_if(is.numeric, function(x) round(x, digits = 2))
  file.remove(f)
  sf::write_sf(site_new, f)
}
all_sites %>% filter(site_name == "tresham") %>% 
  select(percent_commute_active_base:percent_drive_convertable)

# zip(zipfile = "data-sites-2021-02-08.zip", files = "data-small")