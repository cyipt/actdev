# u = "https://github.com/cyipt/actdev/releases/download/0.1.1/LargeApplics.zip"
u = "https://github.com/cyipt/actdev/releases/download/0.1.1/NewLarge.zip"
f = basename(u)
download.file(u, f)
dir.create("data")
# dir.create("data/dump1")
dir.create("data/dump2")
# unzip(f, exdir = "data/dump1/")
unzip(f, exdir = "data/dump2/")
list.files("data/dump2/")

library(tidyverse)

# largeapplics = read_csv("data/dump1/LargeApplics.csv")
largeapplics = read_csv("data/dump2/NewLarge.csv")
largeapplics
sum(is.na(largeapplics$latitude))
sum(is.na(largeapplics$longitude))
summary(largeapplics$latitude)

p = largeapplics %>%
  mutate(across(latitude:longitude, as.numeric)) %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

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
st_join
summary(as.numeric(p$n_dwellings))
