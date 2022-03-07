all_pairs <- read_csv("data/networks/all_pairs_admin2.csv")
sequential <- read_csv("data/networks/sequential_admin2.csv")

distinct_jouneys <- rbind(all_pairs, sequential) %>% 
  select(pcod_from, pcod_to) %>% 
  distinct() %>% 
  mutate(journey = row_number())

a2 <- st_read("data/geo/admin2.geojson") %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  select(-centroid) %>% 
  st_centroid()

network_lines <- distinct_jouneys %>% 
  pivot_longer(!c(journey), values_to = "pcod") %>% 
  left_join(a2 %>% st_centroid(), by = "pcod") %>% 
  st_as_sf() %>% 
  group_by(journey) %>% 
  summarise() %>% st_cast("LINESTRING")

network_lines <- network_lines %>% 
  left_join(distinct_jouneys, by = "journey") %>% 
  select(-journey)

st_write(network_lines, "data/geo/journey_lines.geojson", delete_dsn = T)
