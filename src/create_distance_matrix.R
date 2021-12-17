require(sf)
require(igraph)
require(tidyverse)

admin2 <- st_read("data/geo/admin2.geojson") %>% 
  select(-centroid) %>% 
  slice(1:10)

admin2_cent <- st_centroid(admin2)

st_distance(admin2_cent$geometry, admin2_cent$geometry)

admin2_lookup <- admin2 %>% mutate(index = row_number()) %>% 
  st_drop_geometry()

graph_df <- make_full_graph(length(unique(admin2$pcod))) %>% 
  as_data_frame() %>% as_tibble() %>% 
  left_join(admin2_lookup, by = c("to" = "index")) %>% 
  left_join(admin2_lookup, by = c("from" = "index")) #%>% 
  #slice(1:100)

pt_pairs <- graph_df %>%
  group_by(from, to) %>% 
  left_join(admin2_cent, by = c("pcod.x" = "pcod")) %>% 
  left_join(admin2_cent, by = c("pcod.y" = "pcod")) %>% 
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

graph_df <- graph_df %>% select(-c(from, to)) %>% 
  rename(from = pcod.x,
         to = pcod.y)

write_csv(graph_df, "data/distance/distance_matrix_admin2.csv")
