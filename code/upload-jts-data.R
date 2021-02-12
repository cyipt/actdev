# Downloads geographic data for accessibility stats, and uploads them as releases to the ActDev repo, tag 0.1.2

View(jts_tables)

# LSOA level
all_jts_tables = paste0("jts050", 1:9)
i = all_jts_tables[1]
for(i in all_jts_tables){
  year = 2017
  jts_sf = jts::get_jts_data(table = i,
                             year = year,
                             output_format = "sf")
  f = paste0(i, "-", year, ".geojson")
  sf::write_sf(jts_sf, f)
  piggyback::pb_upload(f, tag = "0.1.2")
  rm(jts_sf)
}

#LA level
all_jts_tables = paste0("jts040", 1:9)
i = all_jts_tables[1]
for(i in all_jts_tables){
  year = 2017
  jts_sf = jts::get_jts_data(table = i,
                             year = year,
                             output_format = "sf")
  f = paste0(i, "-", year, ".geojson")
  sf::write_sf(jts_sf, f)
  piggyback::pb_upload(f, tag = "0.1.2")
  rm(jts_sf)
}

# Correct column names
jts0501 = read_sf("jts0501-2017.geojson")
jts0401 = read_sf("jts0401-2017.geojson")

names(jts0501) = sub("X","", names(jts0501))
names(jts0401) = sub("X","", names(jts0401))

write_sf(jts0501, "jts0501-2017.geojson")
write_sf(jts0401, "jts0401-2017.geojson")

piggyback::pb_upload("jts0501-2017.geojson")
piggyback::pb_upload("jts0401-2017.geojson")
