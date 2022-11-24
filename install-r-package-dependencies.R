# Run this script to install necessary to reproduce results
install.packages("remotes")
cran_pkgs = c("httr", "lwgeom", "mapview", "od", "osmextract", "pct", 
              "remotes", "sf", "stplanr", "sfheaders", "readxl", "zonebuilder", 
              "piggyback", "tmap", "ggplot2", "data.table", "geojsonio", "osmdata", 
              "tidyverse", "cyclestreets", "tmaptools", "spatstat", "rmarkdown", 
              "patchwork", "scales", "colorspace", "jsonlite", "bookdown", 
              "knitr", "xaringan")
remotes::install_cran(cran_pkgs)
remotes::install_github(c("cyipt/acton", "cyipt/actdev"))
remotes::install_github("a-b-street/abstr")
