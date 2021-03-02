# u = "https://github.com/cyipt/actdev/releases/download/0.1.1/LargeApplics.zip"
u = "https://github.com/cyipt/actdev/releases/download/0.1.1/NewLarge.zip"
f = basename(u)
download.file(u, f)
# dir.create("data/dump1")
dir.create("data/dump2")
# unzip(f, exdir = "data/dump1/")
unzip(f, exdir = "data/dump2/")
list.files("data/dump2/")

library(tidyverse)

# largeapplics = read_csv("data/dump1/LargeApplics.csv")
largeapplics = read_csv("data/dump2/NewLarge.csv")
largeapplics
sum(is.na(largeapplics$lat))
sum(is.na(largeapplics$lng))
summary(largeapplics$lat)

p = largeapplics %>%
  mutate(across(lat:lng, as.numeric)) %>% 
  filter(!is.na(lat) & !is.na(lng)) %>% 
  sf::st_as_sf(coords = c("lng", "lat"), crs = 4326)

p %>% sample_n(1000) %>% mapview::mapview()

# extract most common ids to find large application
head(p$associated_id)
summary(nchar(p$associated_id))
pid_table = table(p$associated_id)
summary(pid_table)
head(pid_table)
pid_table = order(pid_table)

tail(pid_table, 4)

national_pct_data = pct::get_pct(geography = "msoa", layer = "z", national = TRUE)

