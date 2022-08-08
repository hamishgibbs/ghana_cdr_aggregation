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
              "output/gravity_modelling/all_pairs_model.rds",
              "output/gravity_modelling/sequential_model.rds",
              "output/gravity_modelling/all_pairs_model_predictions.rds",
              "output/gravity_modelling/sequential_model_predictions.rds",
              "output/figures/movement_raster_comparison.png")
} else {
  .args <- commandArgs(trailingOnly = T)
}

population <- read_csv(.args[1])
distance_matrix <- read_rds(.args[2])

all_pairs <- read_csv(.args[3]) 
sequential <- read_csv(.args[4])

all_pairs_model <- read_rds(.args[5])
sequential_model <- read_rds(.args[6])

all_pairs_prediction <- read_rds(.args[7])
sequential_prediction <- read_rds(.args[8])

metric <- rownames(summary(all_pairs_model)[1,] %>% t())

######
# Compare gravity model parameter estimates

reshape_model_summary <- function(summary){
  return (
    summary %>% 
      as_tibble() %>%
      mutate(rowname = rownames(summary))
  )
}

display_model_summary <- function(summary, tolerance=3){
  return (
    reshape_model_summary(summary) %>% 
      mutate(across(where(is.numeric), round, tolerance)) %>% 
      mutate(display = paste0(mean, " (", Q2.5, ", ", Q97.5, ")")) %>% 
      select(rowname, display)
  )  
}

get_model_summary_difference <- function(summary_1, summary_2, tolerance=3){
  summary_1 <- reshape_model_summary(summary_1)
  summary_2 <- reshape_model_summary(summary_2)
  combined_summaries <- summary_1 %>% left_join(summary_2, by = c("rowname"))
  combined_summaries_display <- combined_summaries %>% 
    mutate(mean_diff = mean.x - mean.y,
           Q2.5_diff = Q2.5.x - Q2.5.y,
           Q97.5_diff = Q97.5.x - Q97.5.y) %>% 
    mutate(across(where(is.numeric), round, tolerance)) %>% 
    mutate(display = paste0(mean_diff, " (", Q2.5_diff, ", ", Q97.5_diff, ")")) %>% 
    select(rowname, display)
  return(combined_summaries_display)
}

display_model_summary(all_pairs_model$summary)
display_model_summary(sequential_model$summary)

get_model_summary_difference(all_pairs_model$summary, sequential_model$summary)

######
# Plot gravity model comparison

a2 <- st_read("data/geo/admin2.geojson") %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  # slice(1) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2")))

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
           mutate(value=as.numeric(value)) %>% 
           # Removes internal population
           filter(pcod_from != pcod_to))
  
}

all_pairs_prediction <- matrix_to_long_df_to_plot(all_pairs_prediction)
sequential_prediction <- matrix_to_long_df_to_plot(sequential_prediction)

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
                                                           limits = c(1, fill_scale_max),
                                                           trans="log10")

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

plot_network_distance_kernel <- function(network, distance_matrix, title=NULL){
  
  journey_distances <- distance_matrix[cbind(network$pcod_from, network$pcod_to)]
  network$distance <- journey_distances
  return (
    network %>% 
      ggplot() + 
      geom_jitter(aes(x = distance, y = value), size=0.01) + 
      scale_y_continuous(trans="log10", labels = scales::comma) + 
      scale_x_continuous(trans="log10") + 
      theme_classic() + 
      labs(x="Distance (km)", y = "Travellers", title=title) 
  )
}

p_all_pairs_kernel <- plot_network_distance_kernel(all_pairs %>% rename(value=value_mean), 
                                                   distance_matrix,
                                                   title="All Pairs (Empirical)")
p_sequential_kernel <- plot_network_distance_kernel(sequential %>% rename(value=value_mean), 
                                                    distance_matrix,
                                                    title="Sequential (Empirical)")
p_all_pairs_prediction_kernel <- plot_network_distance_kernel(all_pairs_prediction, 
                                                              distance_matrix,
                                                              title="All Pairs (Modelled)")
p_sequential_prediction_kernel <- plot_network_distance_kernel(sequential_prediction, 
                                                              distance_matrix,
                                                              title="Sequential (Modelled)")

p_all_pairs <- plot_raster_network(all_pairs %>% rename(value=value_mean), fill_scale=fill_scale, 
                                   name_levels=name_levels,
                                   title="All Pairs (Empirical)")

p_sequential <- plot_raster_network(sequential %>% rename(value=value_mean), fill_scale=fill_scale, 
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
  mutate(perc_difference = ((value_mean.x - value_mean.y) / value_mean.x)*100) %>% 
  drop_na(perc_difference)

prediction_difference <- all_pairs_prediction %>% 
  left_join(sequential_prediction, by=c("pcod_from", "pcod_to")) %>% 
  mutate(perc_difference = ((value.x - value.y) / value.x)*100)

combined_difference_values <- c(empirical_difference$perc_difference, prediction_difference$perc_difference)
difference_breaks <- c(min(combined_difference_values)-1e-7, -25, -15, -10, -5, 0, 5, 10, 15, 25, max(combined_difference_values)+1e-7)

empirical_difference <- ggutils::classify_intervals(empirical_difference, "perc_difference", difference_breaks)
prediction_difference <- ggutils::classify_intervals(prediction_difference, "perc_difference", difference_breaks)

difference_scale_names <- levels(empirical_difference$value)
difference_scale_values <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
names(difference_scale_values) <- difference_scale_names

population_classed <- population %>% 
  mutate(population = cut(population, c(-Inf, 85000, 200000, Inf))) %>% 
  mutate(population = factor(population, 
                             levels=levels(population),
                             labels = c("Low", "Mid", "High")))

population_classed %>% 
  group_by(population) %>% 
  summarise(n = n())

p_population_density <- population %>% 
  ggplot() + 
  geom_density(aes(x = population), fill="grey", alpha = 0.4) + 
  theme_classic() + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) + 
  geom_vline(aes(xintercept=85000), color="red") + 
  geom_vline(aes(xintercept=200000), color="red") + 
  annotate(geom="text", x = 42500, y = 0.00001, label='"Low"\n(n=97)') + 
  annotate(geom="text", x = 142500, y = 0.00001, label='"Mid"\n(n=144)') + 
  annotate(geom="text", x = 419068.3, y = 0.00001, label='"High"\n(n=30)') + 
  labs(x = "Population", y = "Density")

ggsave("output/figures/validation/population_distribition.png",
       p_population_density,
       width=8, height = 5, units="in")

pred_difference_pop_classed <- prediction_difference %>%  
  select(-value.x, -value.y) %>% 
  left_join(population_classed, by = c("pcod_from" = "pcod2")) %>% 
  rename(population_from = population) %>% 
  left_join(population_classed, by = c("pcod_to" = "pcod2")) %>% 
  rename(population_to = population)
  
trip_pop <- mapply(c, pred_difference_pop_classed$population_from, pred_difference_pop_classed$population_to, SIMPLIFY = FALSE)
trip_pop <- lapply(trip_pop, sort)
trip_pop <- lapply(trip_pop, function(x){paste0(x, collapse="_")}) %>% unlist()

pred_difference_pop_classed$trip_pop <- trip_pop

pred_difference_pop_classed %>% 
  group_by(trip_pop) %>% 
  summarise(perc_difference = mean(perc_difference))

p_trip_model_difference <- pred_difference_pop_classed %>% 
  mutate(trip_pop = factor(trip_pop, 
                           levels = rev(c("High_High", "Mid_High", 
                                          "Low_High", "Mid_Mid", 
                                          "Low_Mid", "Low_Low")))) %>% 
  ggplot() + 
  geom_vline(aes(xintercept=0), color="red") + 
  geom_boxplot(aes(x = perc_difference, y = trip_pop, fill=trip_pop),
               outlier.size = 0.2) + 
  theme_classic() + 
  labs(y="Origin and destination populations (re-classified)", x="Difference between models (%)") + 
  scale_fill_manual(values=c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac')) + 
  theme(legend.position = "none")

ggsave("output/figures/p_trip_model_difference.png",
       p_trip_model_difference,
       width=10, height = 5.5, units="in")


p_raster_difference_empirical <- plot_raster_network_difference(empirical_difference, 
                               scale_fill_manual(values = difference_scale_values),
                               name_levels,
                               "Difference (Empirical)")

p_raster_difference_prediction <- plot_raster_network_difference(prediction_difference, 
                               scale_fill_manual(values = difference_scale_values),
                               name_levels,
                               "Difference (Modelled)")

plot_network_distance_kernel_difference <- function(network, distance_matrix, title){
  return(
    plot_network_distance_kernel(network=network, distance_matrix=distance_matrix, 
                                 title=title) + 
      scale_y_continuous() + labs(y = "Difference (%)") + 
      geom_hline(yintercept = 0, color="red", linetype="dashed", size=0.2)
  )
}

p_kernel_difference_empirical <- plot_network_distance_kernel_difference(empirical_difference %>% select(-value) %>% rename(value=perc_difference), 
                                                               distance_matrix,
                                                               title="Difference (Empirical)")

p_kernel_difference_prediction <- plot_network_distance_kernel_difference(prediction_difference %>% select(-value) %>% rename(value=perc_difference), 
                                                               distance_matrix,
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

