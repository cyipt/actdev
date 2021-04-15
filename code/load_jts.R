# jts data - SLOW
all_jts_tables = paste0("jts050", 1:8)
i = all_jts_tables[1]
for(i in all_jts_tables){
  year = 2017
  f = paste0(i, "-", year, ".geojson")
  if(! file.exists(f)) {
    # piggyback::pb_download(f, tag = "0.1.2") # requires piggyback set-up
    # piggyback::pb_download_url(f, tag = "0.1.2") # requires piggyback set-up
    f_url = paste0("https://github.com/cyipt/actdev/releases/download/0.1.2/", f)
    download.file(url = f_url, destfile = f, mode = "wb")
  }
  f2 = sf::read_sf(f)
  assign(i, f2)
  rm(f2)
}