metric <- rownames(summary(all_pairs_model)[1,] %>% t())

rownames(summary(all_pairs_model))

get_model_summary <- function(model){
  return (
    summary(model) %>% 
      as_tibble() %>% round(., 2) %>% 
      mutate(rowname = rownames(summary(model)))
  )
}

display_model_summary <- function(model){
  return (
    get_model_summary(model) %>% 
      mutate(display = paste0(mean, " (", Q2.5, ",", Q97.5, ")")) %>% 
      select(rowname, display)
  )  
}

display_model_summary(all_pairs_model)
display_model_summary(sequential_model)

get_key_difference_metrics <- function(model, postfix) {
  return (
    get_model_summary(model) %>% 
      select(rowname, mean, Q2.5, Q97.5) %>% 
      filter(rowname %in% c("gamma", "omega_1", "omega_2", "theta")) %>% 
      setNames(paste0(names(.), postfix))
  )
}
get_key_difference_metrics(all_pairs_model, "_all_pairs") %>% 
  left_join(get_key_difference_metrics(sequential_model, "_sequential"), 
            by = c("rowname_all_pairs" = "rowname_sequential")) %>% 
  mutate(mean = mean_all_pairs - mean_sequential,
         Q2.5 = Q2.5_all_pairs - Q2.5_sequential,
         Q97.5 = Q97.5_all_pairs - Q97.5_sequential) %>%
  mutate_at(c("mean", "Q2.5", "Q97.5"), round, 2) %>% 
  rename(rowname = rowname_all_pairs) %>% 
  mutate(display = paste0(mean, " (", Q2.5, ",", Q97.5, ")")) %>% 
  select(rowname, display)



#all_pairs_summary %>% mutate(metric) %>% pivot_wider(names_from = metric)

#combined_summary <- cbind(all_pairs_summary, sequential_summary) %>% 
# round(., 3) %>% as_tibble() %>% 
#mutate(metric)

#combined_summary[ , order(names(combined_summary))] %>% view

#summary(mod, probs=c(0.025, 0.975), ac_lags=10)
#check(mod)

#M <- predict(mod)

#M

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

all_pairs_prediction <- matrix_to_long_df_to_plot(predict(all_pairs_model))
sequential_prediction <- matrix_to_long_df_to_plot(predict(sequential_model))

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
    labs(x = NULL, y = NULL, title=title)
  
  return(p)
  
}


p_all_pairs <- plot_raster_network(all_pairs %>% rename(value=value_mean), fill_scale=fill_scale, 
                                   name_levels=name_levels,
                                   title="All Pairs (Observed)")

p_sequential <- plot_raster_network(sequential %>% rename(value=value_mean), fill_scale=fill_scale, 
                                    name_levels=name_levels,
                                    title="Sequential (Observed)")

p_all_pairs_prediction <- plot_raster_network(all_pairs_prediction, fill_scale=fill_scale, 
                                              name_levels=name_levels,
                                              title="All Pairs (Modelled)")
p_sequential_prediction <- plot_raster_network(sequential_prediction, fill_scale=fill_scale,
                                               name_levels=name_levels,
                                               title="Sequential (Modelled)")

p <- cowplot::plot_grid(p_all_pairs, p_sequential, 
                        p_all_pairs_prediction, p_sequential_prediction)


ggsave("output/figures/movement_raster_comparison.png",
       p,
       width=9, height=9, units="in")

