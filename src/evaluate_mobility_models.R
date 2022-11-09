suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(cowplot)
})

if(interactive()){
  .args <-  c("data/population/population_admin2.csv",
              "data/distance/distance_matrix_admin2.rds",
              "data/networks/all_pairs_admin2.csv",
              "data/networks/sequential_admin2.csv",
              "data/mobility_modelling/gravity_exp/all_pairs_model.rds",
              "data/mobility_modelling/gravity_exp/sequential_model.rds",
              "data/mobility_modelling/gravity_exp/all_pairs_model_predictions.rds",
              "data/mobility_modelling/gravity_exp/sequential_model_predictions.rds",
              "data/geo/admin2.geojson",
              "output/figures/movement_raster_comparison.png")
  DIFFERENCE_FILL_SCALE_BREAKS <- as.numeric(stringr::str_split("-25,-15,-10,-5,0,5,10,15,25", pattern=",")[[1]])
  DIFFERENCE_YAXIS_SCALE_BREAKS <- as.numeric(stringr::str_split("0,100,1000,100000", pattern=",")[[1]])
  PLOT_REGLINE_AND_COR_FOR_PREDICTIONS <- 1
} else {
  .args <- commandArgs(trailingOnly = T)
  DIFFERENCE_FILL_SCALE_BREAKS <- as.numeric(stringr::str_split(Sys.getenv("DIFFERENCE_FILL_SCALE_BREAKS"), pattern=",")[[1]])
  DIFFERENCE_YAXIS_SCALE_BREAKS <- as.numeric(stringr::str_split(Sys.getenv("DIFFERENCE_YAXIS_SCALE_BREAKS"), pattern=",")[[1]])
  PLOT_REGLINE_AND_COR_FOR_PREDICTIONS <- as.logical(as.numeric(Sys.getenv("PLOT_REGLINE_AND_COR_FOR_PREDICTIONS")))
}

population <- read_csv(.args[1], col_types = cols())
distance_matrix <- read_rds(.args[2])

all_pairs <- read_csv(.args[3], col_types = cols())
sequential <- read_csv(.args[4], col_types = cols())

all_pairs_model <- read_rds(.args[5])
sequential_model <- read_rds(.args[6])

all_pairs_prediction <- read_rds(.args[7])
sequential_prediction <- read_rds(.args[8])

a2 <- st_read(.args[9]) %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  st_simplify(preserveTopology = T, dTolerance = 100) %>%
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2")))

metric <- rownames(summary(all_pairs_model)[1,] %>% t())

######
# Plot gravity model comparison

a2_lat_long <- a2 %>% select(-centroid, -area) %>%
  st_centroid() %>%
  mutate(lat = unlist(map(geometry,2)),
         long = unlist(map(geometry,1)),
         lat = lat + 0.3)

matrix_to_long_df_to_plot <- function(m){
  df <- data.frame(cbind(pcod_from = rownames(m), m))

  return(df %>%
           pivot_longer(!c(pcod_from), names_to = "pcod_to", values_to = "value") %>%
           drop_na(value) %>%
           mutate(value=as.numeric(value),
                  value = as.integer(value)) %>%
           # Removes internal population
           filter(pcod_from != pcod_to,
                  value > 0))

}

all_pairs_prediction <- matrix_to_long_df_to_plot(all_pairs_prediction)
sequential_prediction <- matrix_to_long_df_to_plot(sequential_prediction)

print(paste("Number of edges (all_pairs):", scales::comma(length(all_pairs_prediction$value))))
print(paste("Number of edges (sequential):", scales::comma(length(sequential_prediction$value))))
print(paste("Difference edges:", scales::percent((length(all_pairs_prediction$value) - length(sequential_prediction$value))/length(all_pairs_prediction$value))))
print(paste("Number of trips (all_pairs):", scales::comma(sum(all_pairs_prediction$value))))
print(paste("Number of trips (sequential):", scales::comma(sum(sequential_prediction$value))))
print(paste("Difference trips:", scales::percent((sum(all_pairs_prediction$value) - sum(sequential_prediction$value))/sum(all_pairs_prediction$value))))

# Total number of daily trips in each network
all_pairs %>%
  select(-value_sum) %>%
  rename(value_all_pairs = value_mean) %>%
  left_join(sequential) %>%
  select(-value_sum) %>%
  rename(value_sequential = value_mean) %>%
  left_join(all_pairs_prediction) %>%
  rename(value_all_pairs_prediction = value) %>%
  left_join(sequential_prediction) %>%
  rename(value_sequential_prediction = value) %>%
  drop_na(c(value_all_pairs, value_all_pairs, value_sequential, value_sequential_prediction)) %>%
  pivot_longer(!c(pcod_from, pcod_to)) %>%
  group_by(name) %>%
  summarise(value = sum(value))

fill_scale_max <- max(c(
  max(all_pairs_prediction$value),
  max(all_pairs$value_mean),
  max(sequential_prediction$value),
  max(sequential$value_mean)
))

fill_scale <- colorspace::scale_fill_continuous_sequential("Teal",
                                                           limits = c(1e-7, fill_scale_max+1e-7),
                                                           trans="log10",
                                                           na.value="white")

name_levels <- data.frame(pcod = population$pcod2) %>%
  left_join(a2_lat_long %>% st_drop_geometry(), by=c("pcod")) %>% arrange(lat) %>%
  pull(pcod)

plot_raster_network <- function(network, fill_scale, name_levels,
                                title){
  
  p <- network %>%
    mutate(pcod_from = factor(pcod_from, levels = name_levels),
           pcod_to = factor(pcod_to, levels = name_levels)) %>%
    ggplot() +
    geom_raster(aes(x = pcod_from, y = pcod_to, fill=value)) +
    fill_scale +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = NULL, y = NULL, title=title) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

  return(p)

}

plot_network_distance_kernel <- function(network,
                                         distance_matrix,
                                         xlimits=NULL,
                                         ylimits=NULL,
                                         title=NULL){

  journey_distances <- distance_matrix[cbind(network$pcod_from, network$pcod_to)]
  network$distance <- journey_distances

  return (
    network %>%
      ggplot(aes(x = distance, y = value)) +
      geom_jitter(size=0.01) +
      scale_y_continuous(trans="log10", labels = scales::comma, limits = ylimits) +
      scale_x_continuous(trans="log10", limits = xlimits) +
      theme_classic() +
      labs(x="Distance (km)", y = "Travellers", title=title)
  )
}

plot_smooth_line_and_equation <- function(){
  return(
    list(geom_smooth(method="lm", level=0.90, size=0.3),
         #ggpubr::stat_regline_equation(label.x.npc = 0.6, label.y.npc = 1, size=3),
         ggpubr::stat_cor(aes(label = ..r.label..), label.x.npc = 0.6, label.y.npc = 0.9, size=3))
  )
}

distance_kernel_ylim <- c(min(c(all_pairs$value_mean, sequential$value_mean, all_pairs_prediction$value, sequential_prediction$value)),
                          max(c(all_pairs$value_mean, sequential$value_mean, all_pairs_prediction$value, sequential_prediction$value)))

print(paste("YLIM", distance_kernel_ylim))

distance_kernel_xlim <- c(1, max(distance_matrix))

p_all_pairs_kernel <- plot_network_distance_kernel(all_pairs %>% rename(value=value_mean),
                                                   distance_matrix,
                                                   ylimits = distance_kernel_ylim,
                                                   xlimits = distance_kernel_xlim,
                                                   title="All Pairs (Empirical)") +
  plot_smooth_line_and_equation()
p_sequential_kernel <- plot_network_distance_kernel(sequential %>% rename(value=value_mean),
                                                    distance_matrix,
                                                    ylimits = distance_kernel_ylim,
                                                    xlimits = distance_kernel_xlim,
                                                    title="Sequential (Empirical)") +
  plot_smooth_line_and_equation()
p_all_pairs_prediction_kernel <- plot_network_distance_kernel(all_pairs_prediction,
                                                              distance_matrix,
                                                              ylimits = distance_kernel_ylim,
                                                              xlimits = distance_kernel_xlim,
                                                              title="All Pairs (Modelled)")

p_sequential_prediction_kernel <- plot_network_distance_kernel(sequential_prediction,
                                                              distance_matrix,
                                                              ylimits = distance_kernel_ylim,
                                                              xlimits = distance_kernel_xlim,
                                                              title="Sequential (Modelled)")

if (PLOT_REGLINE_AND_COR_FOR_PREDICTIONS){
  p_all_pairs_prediction_kernel <- p_all_pairs_prediction_kernel + 
    plot_smooth_line_and_equation()
  p_sequential_prediction_kernel <- p_sequential_prediction_kernel + 
    plot_smooth_line_and_equation()
}

p_all_pairs <- plot_raster_network(all_pairs %>% rename(value=value_mean), 
                                   fill_scale=fill_scale,
                                   name_levels=name_levels,
                                   title="All Pairs (Empirical)")

p_sequential <- plot_raster_network(sequential %>% rename(value=value_mean), 
                                    fill_scale=fill_scale,
                                    name_levels=name_levels,
                                    title="Sequential (Empirical)")

p_all_pairs_prediction <- plot_raster_network(all_pairs_prediction, fill_scale=fill_scale,
                                              name_levels=name_levels,
                                              title="All Pairs (Modelled)")
p_sequential_prediction <- plot_raster_network(sequential_prediction, fill_scale=fill_scale,
                                               name_levels=name_levels,
                                               title="Sequential (Modelled)")
plot_raster_network_difference <- function(network, fill_scale,
                                           name_levels, title){
  return (plot_raster_network(network=network,
                        fill_scale=fill_scale,
                        name_levels=name_levels,
                        title=title) +
            theme(legend.position = "none") +
            labs(fill=NULL)
  )
}

empirical_difference <- all_pairs %>%
  left_join(sequential, by=c("pcod_from", "pcod_to")) %>%
  mutate(difference = value_mean.x - value_mean.y) %>%
  drop_na(difference)

prediction_difference <- all_pairs_prediction %>%
  left_join(sequential_prediction, by=c("pcod_from", "pcod_to")) %>% 
  mutate(difference = value.x - value.y) %>% 
  drop_na(difference)

combined_difference_values <- c(empirical_difference$difference, prediction_difference$difference)

print(paste("Difference min, avg, max:",
  round(min(combined_difference_values), 2),
  round(mean(combined_difference_values), 2),
  round(max(combined_difference_values), 2)))

difference_breaks <- c(min(combined_difference_values)-1e-7, DIFFERENCE_FILL_SCALE_BREAKS, max(combined_difference_values)+1e-7)

empirical_difference <- ggutils::classify_intervals(empirical_difference, "difference", difference_breaks)
prediction_difference <- ggutils::classify_intervals(prediction_difference, "difference", difference_breaks)

difference_scale_names <- levels(empirical_difference$value)
difference_scale_values <- c('#b2182b','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
names(difference_scale_values) <- difference_scale_names

p_raster_difference_empirical <- plot_raster_network_difference(empirical_difference,
                               scale_fill_manual(values = difference_scale_values),
                               name_levels,
                               "Difference (Empirical)")


p_raster_difference_prediction <- plot_raster_network_difference(prediction_difference,
                               scale_fill_manual(values = difference_scale_values),
                               name_levels,
                               "Difference (Modelled)")

plot_network_distance_kernel_difference <- function(network,
                                                    distance_matrix,
                                                    xlimits=NULL,
                                                    ylimits=NULL,
                                                    ybreaks=NULL,
                                                    title){
  return(
    plot_network_distance_kernel(network=network, distance_matrix=distance_matrix,
                                 xlimits=xlimits,
                                 ylimits=ylimits,
                                 title=title) +
      scale_y_continuous(limits = ylimits, trans="pseudo_log", breaks = ybreaks) + labs(y = "Difference") +
      geom_hline(yintercept = 0, color="red", linetype="dashed", size=0.2)
  )
}

difference <- c(empirical_difference$difference, prediction_difference$difference)

difference_ylim <- c(min(difference), max(difference))

p_kernel_difference_empirical <- plot_network_distance_kernel_difference(empirical_difference %>% select(-value) %>% rename(value=difference),
                                                               distance_matrix,
                                                               ylimits = difference_ylim,
                                                               xlimits = distance_kernel_xlim,
                                                               ybreaks = DIFFERENCE_YAXIS_SCALE_BREAKS,
                                                               title="Difference (Empirical)")

p_kernel_difference_prediction <- plot_network_distance_kernel_difference(prediction_difference %>% select(-value) %>% rename(value=difference),
                                                               distance_matrix,
                                                               ylimits = difference_ylim,
                                                               xlimits = distance_kernel_xlim,
                                                               ybreaks = DIFFERENCE_YAXIS_SCALE_BREAKS,
                                                               title="Difference (Modelled)")

titles <- lapply(c("a", "b", "c", "d"), function(x){
  ggdraw() + draw_label(x,x = 0,hjust = 0) + theme(plot.margin = margin(0, 0, 0, 7))})

p <- cowplot::plot_grid(titles[[1]], titles[[2]],
                        titles[[3]], titles[[4]],
                        p_all_pairs, p_all_pairs_prediction,
                        p_all_pairs_kernel, p_all_pairs_prediction_kernel,
                        p_sequential, p_sequential_prediction,
                        p_sequential_kernel, p_sequential_prediction_kernel,
                        p_raster_difference_empirical, p_raster_difference_prediction,
                        p_kernel_difference_empirical, p_kernel_difference_prediction,
                        nrow=4,
                        rel_heights = c(0.05, 0.3, 0.3, 0.3))

legend <- cowplot::get_legend(p_raster_difference_empirical + theme(legend.position = "bottom"))
legend_left <- cowplot::plot_grid(legend, ggplot() + theme(panel.background = element_rect(fill="white")), nrow=1)
p_with_legend <- cowplot::plot_grid(p, legend_left, nrow=2, rel_heights = c(0.92, 0.08))


ggsave(tail(.args, 1),
       p_with_legend,
       width=12, height=9, units="in")
