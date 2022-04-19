require(tidyverse)
require(sf)

a2 <- st_read("data/geo/admin2.geojson") %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  select(-centroid) %>% 
  st_centroid()

distinct_jouneys <- expand.grid(pcod_from=a2$pcod, pcod_to=a2$pcod) %>% 
  as_tibble() %>% mutate_all(as.character) %>% 
  filter(pcod_from != pcod_to) %>% 
  mutate(journey = row_number())

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
