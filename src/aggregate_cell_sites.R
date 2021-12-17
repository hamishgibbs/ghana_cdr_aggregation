require(tidyverse)
require(sf)

cell_sites <- read_csv("data/geo/locations_trips_per_day_lon_lat.csv") %>% 
  separate(pcod, into=c("lat", "lng"), sep=" --- ")

cell_sites <- st_as_sf(cell_sites, coords=c("lat", "lng")) %>% 
  st_set_crs(4326)

admin_boundaries <- st_read("data/geo/admin2.geojson") %>% 
  st_simplify(preserveTopology = T, dTolerance=0.0005)


n_cell_sites <- st_intersection(admin_boundaries, cell_sites) %>% 
  st_drop_geometry() %>% 
  group_by(pcod) %>% 
  summarise(n_cell_sites = n()) %>% 
  rename(pcod2 = pcod)

write_csv(n_cell_sites, "data/cell_sites/cell_sites_admin2.csv")
