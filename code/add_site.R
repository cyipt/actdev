# Aim: Add new case study site for https://actdev.cyipt.bike/ 

## Note: Run this file first 

# Create new directory for case study
dir.create("data-small/exeter-red-cow-village")

# Download geojson polygon of case study area from http://geojson.io/ 
## Note: Ensure file is saved in the new working directory
site = sf::read_sf("data-small/exeter-red-cow-village/exeter-red-cow-village.geojson") 

#Create new site data.frame
                                                        # TODO: should have these columns prepopulated
site$site_name = "exeter-red-cow-village"
site$full_name = "Exeter Red Cow Village (Liveable Exeter)"
site$main_local_authority = "Mid Devon"
site$is_complete = "no"
site$dwellings_when_complete = 664
site$planning_url = "https://www.liveableexeter.co.uk/garden-communities/garden-communities/red-cow-village/"
site$percent_commute_active_base = NA
site$percent_drive_convertable = NA
site$percent_mapped_drive_convertable = NA
site$percent_commute_active_scenario = NA
site$median_commute_distance = NA
site$distance_to_town = NA
site$crossing_points = NA
site$percent_commute_walk_base = NA
site$percent_commute_cycle_base = NA
site$percent_commute_drive_base = NA
site$percent_commute_bus_base = NA
site$percent_commute_rail_base = NA
site$percent_commute_other_base = NA
site$circuity_fast_cycle = NA
site$circuity_walk = NA
site$busyness_fast_cycle = NA
site$in_site_walk_circuity = NA
site$in_site_cycle_circuity = NA
site$in_site_drive_circuity = NA
site$percent_commute_drive_scenario = NA
site$percent_commute_walk_scenario = NA
site$percent_commute_cycle_scenario = NA
site$walk_base_rating = NA
site$cycle_base_rating = NA
site$drive_base_rating = NA

if(!exists("sites")) {
  u = "data-small/all-sites.geojson"
  sites = sf::st_read(u)
}
#Merge new case study with existing case studies
sites = rbind(sites,site)

### Next steps: Run build.r script and generate case study contents
