# Aim: Provide helper methods and functions for adding a new site

add_la = function(site) {
  # Download latest ONS "Local Authority Districts in the United Kingdom" data
  download.file(url = 'https://opendata.arcgis.com/datasets/b7fc294e5c8643f5b506acc2122c6880_0.geojson',
                destfile = 'geojsons/la_boundaries.geojson',
                method = 'wininet')
  # Read input data
  la_ew = sf::read_sf("geojsons/la_boundaries.geojson") %>% sf::as_Spatial() %>% sf::st_as_sf()
  # Create intersection between site and ONS data
  la_site = sf::st_intersection(la_ew, site)
  site$main_local_authority = la_site$LAD19NM
}
