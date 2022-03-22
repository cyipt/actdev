sites = sf::read_sf("data-small/all-sites.geojson")
site_names_to_build = sites$site_name
data_dir = "data-small" # for test sites

# site_name = "exeter-red-cow-village"   # which site to look at (can change)

cents = NULL
for(site_name in site_names_to_build){
  path = file.path(data_dir, site_name)
  site = sf::read_sf(file.path(path, "site.geojson"))
  site_centroid = site %>% 
    st_transform(27700) %>% 
    st_centroid() %>% 
    st_transform(4326) %>% 
    select(site_name)
  cents = rbind(cents, site_centroid)
}

write_csv(cents, "data/cents.csv")

# for (i in 1:nrow(cents)) {
#   coords <- st_coordinates(site_centroid$geometry)
#   coords <- round(coords, 3)
#   st_coordinates(site_centroid$geometry) = round(st_coordinates(site_centroid$geometry), 3) #This is the slow part
# }


