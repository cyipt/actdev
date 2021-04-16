# Aim: create actdev package that people can install

remotes::install_cran("tidytable")
library(tidytable)

pkgs = renv::dependencies()
pkgs_unique = pkgs %>% 
  distinct.(Package, .keep_all = TRUE)
# View(pkgs)
pkgs_dev = pkgs_unique %>% 
  filter.(stringr::str_detect(string = Package, pattern = "abs|jts|ukboundaries|acton|utils|tidyverse"))
pkgs_cran = pkgs_unique %>% 
  filter.(!stringr::str_detect(string = Package, pattern = "abs|jts|ukboundaries|acton|utils|tidyverse"))

remotes::install_cran(pkgs_cran$Package)

usethis::use_description()
for(i in pkgs_cran$Package) {
  usethis::use_package(i)
}
# github pkgs: add manually