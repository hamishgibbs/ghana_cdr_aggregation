require(sf)
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
get_network_mean_degree <- function(network){
  g <- network_to_graph(network)
  return(mean(igraph::degree(g)))
}

get_network_weighted_mean_distance <- function(network){
  distance_assigned <- network %>% 
    left_join(journey_lines %>% 
                mutate(len_km = as.numeric(units::set_units(st_length(geometry), "km"))) %>% 
                st_drop_geometry(),
              by=c("pcod_from", "pcod_to"))
  
  return(weighted.mean(distance_assigned$len_km, distance_assigned$value_sum))
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

a2 <- st_read("data/geo/admin2.geojson") %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  # slice(1) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2")))

area <- a2 %>% st_drop_geometry() %>% 
  select(pcod, area) %>% 
  rename(pcod2 = pcod)
#a2 %>% ggplot() + geom_sf()

journey_lines <- st_read("data/geo/journey_lines.geojson")

all_pairs_edges <- get_edge_number(all_pairs)
sequential_edges <- get_edge_number(sequential)

all_pairs_trips <- get_trip_number(all_pairs)
sequential_trips <- get_trip_number(sequential)

all_pairs_mean_degree <- get_network_mean_degree(all_pairs)
sequential_mean_degree <- get_network_mean_degree(sequential)

all_pairs_density <- get_network_edge_density(all_pairs)
sequential_density <- get_network_edge_density(sequential)

all_pairs_weighted_mean_distance <- get_network_weighted_mean_distance(all_pairs)
sequential_weighted_mean_distance <- get_network_weighted_mean_distance(sequential)

total_trips_from_origin <- function(network, type){
  return (network %>% 
    group_by(pcod_from) %>% 
    summarise(value = sum(value_mean)) %>% 
    left_join(area, by = c("pcod_from" = "pcod2")) %>% 
    left_join(population, by = c("pcod_from" = "pcod2")) %>% 
    left_join(cell_sites, by = c("pcod_from" = "pcod2")) %>% 
    mutate(type = type))
}

# TODO: CLEAN THIS UP. REMOVE DUPLICATION.
ds <- total_trips_from_origin(sequential, type="Sequential") 
da <- total_trips_from_origin(all_pairs, type="All Pairs")

color_scale <- scale_color_manual(values = c("All Pairs" = "red", "Sequential" = "black"))
point_plot_theme <- theme(legend.title = element_blank(),
                          legend.position = c(0.83, 0.2),
                          legend.background = element_blank())
point_size <- 0.2

p_trips_out_pop <- rbind(ds, da) %>% 
  ggplot() + 
  geom_point(aes(x = population / area, y = value, color=type), size=point_size) + 
  scale_x_continuous(trans = "log10", labels = scales::comma) + 
  scale_y_continuous(trans = "log10", labels = scales::comma) + 
  color_scale + 
  labs(x = bquote("Origin Population per "~km^2), y = "Average Daily Outbound Trips",
       title = "a") + 
  theme_classic() + 
  point_plot_theme

p_trips_cell_sites <- rbind(ds, da) %>% 
  ggplot() + 
  geom_point(aes(x = n_cell_sites, y = value, color=type), size=point_size) + 
  color_scale + 
  scale_y_continuous(trans = "log10", labels = scales::comma) + 
  labs(x = "Origin Cell Site Count", y = "Average Daily Outbound Trips",
       title = "b") + 
  theme_classic() + 
  point_plot_theme


combined_networks <- rbind(all_pairs, sequential)

size_scale <- scale_size_continuous(range=c(0.01,1), 
                      limits = c(min(combined_networks$value_mean), 
                                 max(combined_networks$value_mean)))

plot_network <- function(network, size_scale, a2){
  return (network %>% 
            left_join(journey_lines, by = c("pcod_from", "pcod_to")) %>% 
            st_as_sf() %>% 
            ggplot() + 
            geom_sf(data=a2, color="black", fill="white", size=0.2) +
            geom_sf(aes(size = value_mean)) + 
            size_scale +
            theme_void() + 
            theme(legend.position = "none",
                  plot.background = element_rect(fill = "white", color="transparent")))
}
annotation_pos <- list(x=-1, y=10, size=3, hjust=0)

p_net_ap <- plot_network(all_pairs, size_scale, a2) + 
  labs(title="c", subtitle = "All Pairs Aggregation")
  
p_net_s <- plot_network(sequential, size_scale, a2) + 
  labs(title="d", subtitle = "Sequential Aggregation")
  
p_trips <- cowplot::plot_grid(p_trips_out_pop, p_trips_cell_sites, ncol = 1)

p <- cowplot::plot_grid(p_trips, p_net_ap, p_net_s, ncol = 3,
                        rel_widths = c(0.38, 0.31, 0.31))

ggsave("output/figures/figure_1.png", 
       p,
       width = 10, height=5, units='in')
            