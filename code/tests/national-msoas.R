# zones = pct::get_pct(geography = "msoa", layer = "z", national = TRUE)
zones = sf::st_as_sf(readRDS("~/npct/pct-outputs-national/commute/msoa/z_all.Rds"))
names(zones)
zones_core = zones[1:13]
names(zones_core)

od_data = pct::get_od()

summary(od_data$geo_code1 %in% zones$geo_code)
summary(od_data$geo_code2 %in% zones$geo_code)
names(od_data)
readr::write_csv(od_data, "od_data.csv")

sf::write_sf(zones_core, "zones_core.geojson")
piggyback::pb_upload(file = "od_data.csv", repo = "cyipt/actdev")
piggyback::pb_download_url(file = "od_data.csv", repo = "cyipt/actdev")

# get from dkan:
u = "https://s3-eu-west-1.amazonaws.com/statistics.digitalresources.jisc.ac.uk/dkan/files/FLOW/wu03uk_v3/wu03uk_v3.csv"
od_data_dk = readr::read_csv(u)
nrow(od_data) / nrow(od_data_dk)
