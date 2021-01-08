# Aim: get relevant summaries at LA level

library(acton)
library(tidyverse)
library(sf)

x = jts_tables$table_title
remove_jts_content = function(
  x,
  p1 = "Travel time, destination and origin indicators ",
  p2 = " by mode of travel",
  p3 = " and local authority, England+.*",
  p4 = ", Lower Super Output Area \\(LSOA\\), England+.*",
  p5 = " by cycle and car, local authority, England",
  p6 = "to |for "
  ) {
  x = gsub(pattern = p1, replacement = "", x = x, fixed = TRUE)
  x = gsub(pattern = p2, replacement = "", x = x)
  x = gsub(pattern = p3, replacement = "", x = x)
  x = gsub(pattern = p4, replacement = "", x = x)
  x = gsub(pattern = p5, replacement = "", x = x)
  x = gsub(pattern = p6, replacement = "", x = x)
  x
}

remove_jts_content(x)

jts_tables_updated = jts_tables %>% 
  mutate(service = remove_jts_content(table_title))
jts_tables_updated

tables_to_get = jts_tables_updated %>% 
  filter(str_detect(csv_file_name, "jts040[1-9]-2017")) %>% 
  select(table_code, service)
tables_to_get

jtst = paste0("jts040", 1:9)
# jts_data1 = get_jts_data(jtst[1], year = 2017, skip = 5)
jts_data1 = get_jts_data(jtst[1], year = 2017)
nm = names(jts_data1)

jts_mode_names = c("PT", "Cyc", "Car")
jtsm_collapsed = paste0(jts_mode_names, collapse = "|")
nm[!grepl(pattern = jtsm_collapsed, x = nm)]

jtsmt = paste0(jts_mode_names, "t")
jtsmt_collapsed = paste0(jtsmt, collapse = "|")
jts_to_join = jts_data1 %>% 
  select(1:4, matches(jtsmt_collapsed) & matches("5000"))

# get geo data
lads = ukboundaries::lad2011_simple
summary(not_in_lads <- lads$code %in% jts_data1$LA_Code)
summary(not_in_jts <- jts_data1$LA_Code %in% lads$code)
lads_to_join = lads %>% rename(LA_Code = code)

jts_sf = inner_join(lads_to_join, jts_to_join)
plot(jts_sf %>% select(-(1:6)))

# next-up: write function to do that for all data

lookup_table = function(from, x, y) {
  names(y) = x
  unname(y[from])
}
jts_mode_names_full = c("Public transport", "Cycle", "Car")
lookup_table(rep(jts_mode_names, 2), jts_mode_names, y)

jts_to_modes = function(from) {
  lookup_table(from = from, x = jts_mode_names, y = jts_mode_names_full)
}

jts_to_modes("Cyc")
jts_to_modes(c("Cyc", "Cyc"))

jts_to_purpose = function(from) {
  lookup_table(from = from, x = tables_to_get$table_code, y = tables_to_get$service)
}

jts_to_purpose(tables_to_get$table_code)


x = jts_data1
jts_to_purpose(jtst)
x = get_jts_data(jtst[2], year = 2017)
x = jtst[5] # GP surgery data fails for some reason
x = jtst[9] # fails
clean_jts = function(x, to_join = lads_to_join) {
  if(is(x, "character")) {
    message("Getting data for ", x, " ", jts_to_purpose(x))
    if(grepl(pattern = "405|406|409", x = x)) {
      x = get_jts_data(x, year = 2017, skip = 7)
    } else {
      x = get_jts_data(x, year = 2017)
    }
  }
  nms = names(x)
  if(any(grepl(pattern = "5000", x = nms))) {
    x = x %>% select(1:4, matches(jtsmt_collapsed) & matches("5000"))
  }
  else {
    x = x %>% select(1:4, matches(jtsmt_collapsed))
  }
  names(x)[(ncol(x)-2):(ncol(x))] = jts_mode_names_full
  xj = inner_join(to_join, x)
  names(xj)
  xj
}

jts_list_las = lapply(jtst, clean_jts)
jts_list_las = as.list(jts_to_purpose(tables_to_get$table_code))
i = 3
for(i in 1:length(jts_list_las)) {
  jts_list_las[[i]] = clean_jts(x = )
}

lads$name[!not_in_lads]
jts_data1$LA_Name[!not_in_jts]
# 38 UK counties
jts_data1 %>% 
  filter(str_detect(string = LA_Name, pattern = "Birm")) %>% 
  select(1:4)


# works but not easy to use
jtst = paste0("jts050", 1:9)
jts_data1 = get_jts_data(jtst[1], year = 2017)
jts_data = purrr::map_dfr(jtst, acton::get_jts_data, year = 2017, .id = "table")
jts_data
summary(jts_data)
