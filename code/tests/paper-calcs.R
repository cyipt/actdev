
# within site circuity ----------------------------------------------------


sites_join = sf::read_sf("data-small/all-sites.geojson")

circ_stats = sites_join %>% 
  filter(! is.na(in_site_walk_circuity),
         site_name != "micklefield") %>% 
  mutate(across(c(in_site_walk_circuity,in_site_cycle_circuity,in_site_drive_circuity), as.numeric))

mean(circ_stats$in_site_walk_circuity)
mean(circ_stats$in_site_cycle_circuity)
mean(circ_stats$in_site_drive_circuity)

circ_stats = circ_stats %>% 
  mutate(walk_drive_ratio = in_site_walk_circuity/in_site_drive_circuity,
         cycle_drive_ratio = in_site_cycle_circuity/in_site_drive_circuity)

mean(circ_stats$walk_drive_ratio)
mean(circ_stats$cycle_drive_ratio)

# mostly complete
incomplete = c("partly", "partly (partly before 2011)")
circ_complete = sites_join %>% 
  filter(! is.na(in_site_walk_circuity),
         ! is_complete %in% incomplete) %>% 
  mutate(across(c(in_site_walk_circuity,in_site_cycle_circuity,in_site_drive_circuity), as.numeric))

mean(circ_complete$in_site_walk_circuity)
mean(circ_complete$in_site_cycle_circuity)
mean(circ_complete$in_site_drive_circuity)

circ_complete = circ_complete %>% 
  mutate(walk_drive_ratio = in_site_walk_circuity/in_site_drive_circuity,
         cycle_drive_ratio = in_site_cycle_circuity/in_site_drive_circuity)

mean(circ_complete$walk_drive_ratio)
mean(circ_complete$cycle_drive_ratio)

summary(sites_join$crossing_points)


# figure 1 ----------------------------------------------------------------


library(tidyverse)
beforechanges = read_csv("data/beforechanges.csv")
afterchanges = read_csv("data/afterchanges.csv")

withdwellings = afterchanges %>% 
  filter(! is.na(n_dwellings)) %>% 
  mutate(n_dwellings = as.integer(n_dwellings))

beforechanges$app_size = case_when(
  is.na(beforechanges$app_size) ~ "Other",
  beforechanges$app_size == "Small" ~ "Other",
  TRUE ~ beforechanges$app_size
)

afterchanges$app_size = case_when(
  is.na(afterchanges$app_size) ~ "Other",
  afterchanges$app_size == "Small" ~ "Other",
  TRUE ~ afterchanges$app_size
)

bc = beforechanges %>% 
  group_by(app_size) %>% 
  summarise(n = n()) %>% 
  mutate(status = "Before")

ac = afterchanges %>% 
  group_by(app_size) %>% 
  summarise(n = n()) %>% 
  mutate(status = "After")

ad = rbind(bc, ac)
ad$status = factor(ad$status, levels = c("Before", "After"))

ggplot(ad, aes(fill = status, y = n, x = app_size)) + 
  geom_bar(position = "dodge", stat="identity") +
  scale_fill_manual(values = c("#90be6d", "#457b9d")) +
  labs(x = "App size", y = "", fill = "") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size = 12))

# "#457b9d", "#90be6d", "#ffd166", "#fe5f55"

isna = afterchanges %>% 
  filter(is.na(app_size))

unique(isna$n_documents)
unique(isna$n_dwellings)
unique(isna$n_statutory_days)
