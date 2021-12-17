require(sf)
require(igraph)
require(tidyverse)

admin2 <- st_read("data/geo/admin2.geojson") %>% 
  select(-centroid)

admin2_cent <- st_centroid(admin2)

graph_df <- t(combn(as.character(admin2$pcod), 2)) %>% 
  as_tibble() %>% 
  rename(from=V1, to=V2) #%>% 
  #slice(1:100)

pt_pairs <- graph_df %>% 
  left_join(admin2_cent, by = c("from" = "pcod")) %>% 
  left_join(admin2_cent, by = c("to" = "pcod")) %>% 
  group_by(from, to) %>% 
  group_split()

i <- 1
distances <- c()

for (pair in pt_pairs){
  d <- sf::st_distance(pair$geometry.x, pair$geometry.y)
  d <- round(as.numeric(units::set_units(d, "km")), 2)
  distances <- append(distances, d)
  print(round(i / length(pt_pairs), 2))
  i <- i + 1
}

graph_df$distance <- distances

write_csv(graph_df, "data/distance/distance_matrix_admin2.csv")
