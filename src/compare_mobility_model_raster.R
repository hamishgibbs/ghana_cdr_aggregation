suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(cowplot)
})

if(interactive()){
  .args <-  c("data/mobility_modelling/mobility_model_predictions.csv",
              "data/geo/admin2.geojson",
              "output/figures/movement_raster_comparison.png")
} else {
  .args <- commandArgs(trailingOnly = T)
}

kernels <- read_csv(.args[1], col_types = cols())

a2 <- st_read(.args[2]) %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  st_simplify(preserveTopology = T, dTolerance = 100) %>%
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2")))

# pick up here for unified raster plot

######
# Plot gravity model comparison

a2_lat_long <- a2 %>% select(-centroid, -area) %>%
  st_centroid() %>%
  mutate(lat = unlist(map(geometry,2)),
         long = unlist(map(geometry,1)),
         lat = lat + 0.3)


kernels$model <- factor(kernels$model, levels = c("empirical", "gravity_exp", "gravity_power", "radiation_basic"),
                        labels = c("Empirical", "Gravity (Exponential)", "Gravity (Power)", "Radiation"))
kernels$network_type <- factor(kernels$network_type, levels = c("all_pairs", "sequential"),
                               labels = c("All Pairs", "Sequential"))

plot_raster_network <- function(network, name_levels){

  p <- network %>%
    mutate(pcod_from = factor(pcod_from, levels = name_levels),
           pcod_to = factor(pcod_to, levels = name_levels)) %>%
    ggplot() +
    geom_raster(aes(x = pcod_from, y = pcod_to, fill=value)) +
    colorspace::scale_fill_continuous_sequential("Hawaii", trans="log10",
                                                 labels = scales::comma) +
    theme_classic() +
    facet_grid(network_type~model) +
    theme(legend.position = "right",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size=10)) +
    labs(x = NULL, y = NULL, fill="Daily Trips")
  return(p)
}

name_levels <- data.frame(pcod = unique(kernels$pcod_from)) %>%
  left_join(a2_lat_long %>% st_drop_geometry(), by=c("pcod")) %>% arrange(lat) %>%
  pull(pcod)

p <- plot_raster_network(kernels, name_levels=name_levels)

ggsave(tail(.args, 1),
       p,
       width=10, height=5, units="in")
