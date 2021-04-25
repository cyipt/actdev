# Aim: Programtically add LA's to site data
# Inputs: ActDev site data & ONS local authority boundaries
# Method: Loop through site and ONS sf_intersections
# Libraries:
library(sf)

if (file.exists("la_boundaries.geojson")) {
  file.remove("la_boundaries.geojson")
  # Download latest ONS "Local Authority Districts in the United Kingdom" data
  download.file(url = 'https://opendata.arcgis.com/datasets/b7fc294e5c8643f5b506acc2122c6880_0.geojson',
                destfile = 'la_boundaries.geojson',
                method = 'wininet')
  # Read input data
  la_ew = sf::read_sf("la_boundaries.geojson") %>% sf::as_Spatial() %>% sf::st_as_sf()
  sites = sf::read_sf("data-small/all-sites.geojson")
  # Transform Data (and drop great-kneighton & Trumpington Meadows something funky going on there)
  sites_geo = sites[1] %>% sf::st_as_sf()
  sites_geo = sites_geo[-c(18, 36), ]
  # Loop through sites and calculate main_local_authority
  for (i in 1:nrow(sites_geo)) {
    row <- sites_geo[i, ]
    row_name = row$site_name
    la_site = sf::st_intersection(la_ew, row)
    sites$main_local_authority[sites$site_name == row_name] = la_site$LAD19NM
  }
  # Write out sf
  file.remove("data-small/all-sites.geojson")
  sf::write_sf(sites, "data-small/all-sites.geojson")
}