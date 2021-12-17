require(sf)
require(igraph)
require(tidyverse)

admin2 <- st_read("data/geo/admin2.geojson") %>% 
  select(-centroid)

admin2_cent <- st_centroid(admin2)

distance_matrix <- st_distance(admin2_cent$geometry, admin2_cent$geometry)

distance_matrix <- units::set_units(distance_matrix, "km")

write_rds(distance_matrix, "data/distance/distance_matrix_admin2.rds")
