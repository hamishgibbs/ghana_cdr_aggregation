require(sf)

introduction_pcods <- read_rds("output/modelling/introduction_pcods.rds")
a2 <- st_read("data/geo/admin2.geojson") %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  # slice(1) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2")))

read_introduction_data <- function(introduction_location){
  introduction_sensitivity <- list()
  i <- 1
  for (R0 in c(1.1, 1.5, 3.0)){
    for (mobility_type in c("all_pairs", "sequential")){
      introductions_path <- paste0("output/modelling/preliminary/", mobility_type, "/")
      introductions_fn <- paste0(introductions_path, "infected_", introduction_location, "_R0_", R0, "_trajectory.rds")
      print(introductions_fn)
      introductions <- list.files(introductions_path)
      result_data <- read_rds(introductions_fn)
      result_data$mobility_type <- mobility_type
      result_data$introduction_location <- introduction_location
      result_data$R0 <- R0
      introduction_sensitivity[[i]] <- result_data
      i <- i+1
    }
  }
  return(do.call(rbind, introduction_sensitivity))
}

introduction_sensitivity <- introduction_sensitivity %>% 
  mutate(mobility_type = factor(mobility_type, levels=c("all_pairs", "sequential"),
                                labels = c("All Pairs", "Sequential")))

plot_arrival_timing_difference <- function(introduction_sensitivity, axis_text_size){
  
  introduction_comparison <- introduction_sensitivity %>% 
    group_by(node, R0, mobility_type) %>% 
    mutate(introduction_date = I >= 1 & !duplicated(I >= 1)) %>% 
    filter(introduction_date) %>% 
    select(-c(S, E, I, R, introduction_date, introduction_location)) %>% 
    pivot_wider(names_from=mobility_type, values_from=time)
  
  p <- introduction_comparison %>% ggplot() + 
    geom_point(aes(x = `All Pairs`, y = Sequential), size=0.4) + 
    geom_abline() + 
    facet_wrap(~R0, scales="free") + 
    theme_classic() + 
    theme(plot.margin = unit(c(0, 0.1, 0, 0), "null"),
          axis.title = element_text(size=axis_text_size)) + 
    labs(x="Arrival Time (All Pairs)", y="Arrival Time (Sequential)")
  
  return(p)
}

plot_peak_infections <- function(introduction_sensitivity, axis_text_size){
  p <- introduction_sensitivity %>% 
    group_by(node, R0, mobility_type) %>% 
    filter(I == max(I)) %>% 
    ggplot() + 
    geom_boxplot(aes(x = mobility_type, y = I, fill=mobility_type),
                 outlier.size=0.3, width=0.5, size=0.3) + 
    facet_wrap(~R0) + 
    scale_y_continuous(trans="log10", labels = scales::comma) + 
    theme_classic() + 
    theme(legend.position = "none", 
          plot.margin = unit(c(0, 0, 0, 0.05), "null"),
          axis.title = element_text(size=axis_text_size)) + 
    labs(x = NULL, y = "Peak Number of Infections")
  
  return(p)
}

axis_text_size <- 10
introduction_names <- c("Accra", "Kumasi", "Tamale")

introduction_plots <- list()

for (introduction_location in c(1, 2, 3)){

  introduction_sensitivity <- read_introduction_data(introduction_pcods[introduction_location]) %>% 
    mutate(mobility_type = factor(mobility_type, levels=c("all_pairs", "sequential"),
                                labels = c("All Pairs", "Sequential")),
           R0 = paste0("R0 = ", R0))
  
  p_arrival <- plot_arrival_timing_difference(introduction_sensitivity,
                                              axis_text_size=axis_text_size)
  p_infection <- plot_peak_infections(introduction_sensitivity,
                                      axis_text_size=axis_text_size)
  
  title_gg <- ggplot() + 
    labs(title = paste0("Epidemic seeded in ", introduction_names[introduction_location])) + 
    theme(plot.title = element_text(size=12))
  
  p <- cowplot::plot_grid(p_arrival, p_infection, ncol=2)
  p <- cowplot::plot_grid(title_gg, p, ncol = 1, rel_heights = c(0.15, 1))
  
  introduction_plots[[introduction_location]] <- p
  
}

p_introductions <- cowplot::plot_grid(plotlist = introduction_plots, nrow = 3)

introduction_locations <- a2 %>% filter(pcod %in% names(introduction_pcods)) %>% 
  mutate(name = introduction_names) %>% select(-centroid) %>% 
  st_centroid() %>% 
  mutate(lat = unlist(map(geometry,2)),
         long = unlist(map(geometry,1)),
         lat = lat + 0.3)

p_map <- a2 %>% 
  ggplot() + 
  geom_sf(size = 0.1) +
  geom_label(data=introduction_locations, aes(long, lat, label=name)) + 
  geom_sf(data = introduction_locations) + 
  theme_void()

p <- cowplot::plot_grid(p_introductions, p_map, ncol=2, rel_widths = c(0.7, 0.3))

ggsave("output/figures/introduction_comparison.png",
       p, 
       width=14, height=7, units='in')

