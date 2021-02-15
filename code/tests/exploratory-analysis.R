
library(ggplot2)

dart = read_sf("data-small/lcid/dartboard.geojson")
routes_fast = read_sf("data-small/lcid/routes-fast.geojson")
site = read_sf("data-small/lcid/site.geojson")
mode_split = read_csv("data-small/lcid/mode-split.csv")
all_od = read_csv("data-small/great-kneighton/all-census-od.csv")

mapview::mapview(dart["circuity_walk"]) +
  mapview(routes_fast["all_base"])

summary(all_od)
mean(all_od$length)
max(all_od$length)
# weighted.mean(all_od$length, w = all_od$all)

sum(all_od$all)
dim(all_od)

sum(routes_fast$all_base)

# create stacked bar chart of baseline commute mode / distance from `mode_split`
# with a bar for each distance category, containing stacked travel modes
# walking/cycling in high distance bands is likely to be an unrealistic side effect of using data from large MSOAs  

mode_split$other_base = mode_split$all_base - mode_split$walk_base - mode_split$cycle_base - mode_split$drive_base
mode_long = pivot_longer(mode_split, cols = c(walk_base, cycle_base, drive_base, other_base))
iris$Species <- factor(iris$Species, levels = c("virginica", "versicolor", "setosa"))
mode_long$name = factor(mode_long$name, levels = c("other_base", "drive_base", "cycle_base", "walk_base"))

ggplot(mode_long, aes(fill = name, y = value, x = length_cat)) +
  geom_bar(position = "stack", stat = "identity")

