require(igraph)
require(tidyverse)

get_edge_number <- function(network){
  return(
    network %>% mutate(edge = paste0(pcod_from, pcod_to)) %>%
      pull(edge) %>% unique() %>% length())
}
get_trip_number <- function(network){
  return(
    network %>% pull(value_sum) %>% sum()
  )
}
network_to_graph <- function(network){
  network_data <- network %>% 
    rename(from = pcod_from,
           to = pcod_to,
           weight = value_sum)
  return(graph_from_data_frame(network_data))
}
get_network_edge_density <- function(network){
  g <- network_to_graph(network)
  return(edge_density(g))
}
get_percentage_difference <- function(a, b){
  return(
    ((b - a) / a) * 100
  )
}

all_pairs <- read_csv("data/networks/all_pairs_admin2.csv")
sequential <- read_csv("data/networks/sequential_admin2.csv")

population <- read_csv("data/population/population_admin2.csv")
cell_sites <- read_csv("data/cell_sites/cell_sites_admin2.csv")

all_pairs_edges <- get_edge_number(all_pairs)
sequential_edges <- get_edge_number(sequential)
get_percentage_difference(all_pairs_edges, sequential_edges)

all_pairs_trips <- get_trip_number(all_pairs)
sequential_trips <- get_trip_number(sequential)
get_percentage_difference(all_pairs_trips, sequential_trips)

all_pairs_density <- get_network_edge_density(all_pairs)
sequential_density <- get_network_edge_density(sequential)
get_percentage_difference(all_pairs_density, sequential_density)

# TODO: CLEAN THIS UP. REMOVE DUPLICATION.
ds <- sequential %>% 
  group_by(pcod_from) %>% 
  summarise(value = sum(value)) %>% 
  left_join(population, by = c("pcod_from" = "pcod2")) %>% 
  left_join(cell_sites, by = c("pcod_from" = "pcod2"))

da <- all_pairs %>% 
  group_by(pcod_from) %>% 
  summarise(value = sum(value)) %>% 
  left_join(population, by = c("pcod_from" = "pcod2")) %>% 
  left_join(cell_sites, by = c("pcod_from" = "pcod2"))

ggplot() + 
  geom_point(data = da, aes(x = population, y = value), color="red") + 
  geom_point(data = ds, aes(x = population, y = value)) + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10")

ggplot() + 
  geom_point(data = da, aes(x = n_cell_sites, y = value), color="red") + 
  geom_point(data = ds, aes(x = n_cell_sites, y = value)) + 
  scale_y_continuous(trans = "log10")


all_pairs %>% 
  ggplot() +
  geom_raster(aes(x = pcod_from, y = pcod_to, fill = value))
