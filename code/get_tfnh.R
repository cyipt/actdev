# Aim: get sf object with all TfNH boundaries that have been digitised

# see https://github.com/cyipt/actdev/issues/22

# get geojson for one site, e.g.
# https://gist.github.com/joeytalbot/9cf8c15ce9142609c9611abf9fb7d8bc

u = "https://gist.github.com/joeytalbot/9cf8c15ce9142609c9611abf9fb7d8bc"
u_json = "https://gist.github.com/joeytalbot/9cf8c15ce9142609c9611abf9fb7d8bc/raw/6ec5cf8ae19db6366db60ed242fc8a9c341979ef/map.geojson"
boundary = sf::read_sf(u_json)
mapview::mapview(boundary)

# workflow to get them from all sites, possibly using this table:
# https://gist.github.com/aspeakman/989994ec957da57640610d9aa1cd0939
