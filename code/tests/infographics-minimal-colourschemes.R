# Aim: highlight different colourscheme options for pngs of infographics

# run from route directory of actdev repo
setwd("~/cyipt/actdev")
source("code/mode-split-summary.R")

ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("blue", "green", "purple", "red")) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()

ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkblue", "blue", "purple", "red")) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()

# alternative palette - see
# https://www.colourlovers.com/palette/4790872/DianaSpringCrocus
cols = c("#698203", "#9FC1DA", "#D9BEDF", "#D38521", "#698203", "#9FC1DA", "#D9BEDF", "#D38521")
ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()

# https://www.colourlovers.com/palette/92095/Giant_Goldfish
cols = names(read.csv(text = "#69D2E7,#A7DBD8,#E0E4CC,#F38630,#FA6900,#69D2E7,#A7DBD8,#E0E4CC", check.names = F))
cols = cols[-3]
ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()

# with grey background
ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#434343"))

# with transparent background
g = ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()
ggsave(filename = "test.png", width = 4, height = 3, dpi = 100, plot = g2, bg = "transparent")
# i = magick::image_read("test.png")
# i
# i2 = magick::image_background(i, color = "purple")
# i2

# abstr colours
cols = c("#DF8C3D", "#5D9630", "#12409D", "#A32015")
g = ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()
g

cols = c("#457b9d", "#90be6d", "#ffd166", "#fe5f55")
g = ggplot(all_dist, aes(fill = name, y = value, x = distance_band)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(y = "", x = "Distance band (km)", fill = "") +
  theme_minimal()
g
