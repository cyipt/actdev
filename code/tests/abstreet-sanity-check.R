# Aim: sanity check for A/B Street Scenarios

# There 289 finished trips under the Go Active scenario for Allerton Bywater
l = sf::read_sf("data-small/allerton-bywater/desire-lines-few.geojson")
sum(l$trimode_base) # 289 trips
