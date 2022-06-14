require(tidyverse)
require(sf)

if (interactive()){
  .args <- c(
    "data/modelling/recoded_pcod.rds",
    "data/modelling/population.rds",
    "data/geo/admin2.geojson",
    "data/geo/pcods_admin2.csv",
    "output/modelling/preliminary/peak_infections.csv",
    "output/modelling/preliminary/arrival_times.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

pcods <- read_rds(.args[1])
population <- read_rds(.args[2])

a2 <- st_read(.args[3])
pcods_a2 <- read.csv(.args[4], header = F, col.names = c("pcod", "name2"))

a2 <- a2 %>% left_join(pcods_a2, by =c("pcod"))
a2 <- a2 %>% mutate(geometry = st_make_valid(geometry)) %>% 
  st_simplify(dTolerance = 100)

mobility_type_levels <- c("all_pairs", "sequential")
mobility_type_labels <- c("All Pairs", "Sequential")
name2_levels <- c("LAWRA", "TM-TAMALE SOUTH", "NKWANTA SOUTH", "KMA-MANHYIA SOUTH", "AMA-OKAIKOI SOUTH")

assign_factor_levels <- function(data, 
                                 pcods_a2,
                                 mobility_type_levels,
                                 mobility_type_labels,
                                 name2_levels){
  return(data %>% left_join(pcods_a2, by =c("introduction_location" = "pcod")) %>% 
    mutate(mobility_type = factor(mobility_type, levels=mobility_type_levels, 
                                  labels = mobility_type_labels),
           name2 = factor(name2, levels = name2_levels)))
}

peak_infections <- read_csv(.args[5]) %>% 
  assign_factor_levels(pcods_a2,
                       mobility_type_levels,
                       mobility_type_labels,
                       name2_levels)
arrival_times <- read_csv(.args[6]) %>% 
  assign_factor_levels(pcods_a2,
                       mobility_type_levels,
                       mobility_type_labels,
                       name2_levels)

plot_peak_infections_facet_by_R0 <- function(facet_R0,
                                             mobility_type_levels,
                                             mobility_type_labels,
                                             name2_levels){
  peak_infections %>% 
    filter(R0 == facet_R0) %>% 
    group_by(R0, mobility_type, name2) %>% 
    mutate(maxI=maxI/1000) %>% 
    summarise(maxI_mean = mean(maxI),
              maxI_min = min(maxI),
              maxI_0.25 = quantile(maxI, 0.25),
              maxI_0.75 = quantile(maxI, 0.75),
              maxI_max = max(maxI)) %>% 
    ggplot() + 
    geom_segment(aes(x = maxI_min, xend=maxI_max, y=mobility_type, yend=mobility_type,
                     color=mobility_type),
                 size=3, alpha=0.4) + 
    geom_segment(aes(x = maxI_0.25, xend=maxI_0.75, y=mobility_type, yend=mobility_type,
                     color=mobility_type),
                 size=3, alpha=0.4) + 
    geom_point(aes(x = maxI_mean, y=mobility_type,
                   color=mobility_type),
               size=0.3) + 
    facet_grid( R0 ~ name2) + 
    theme_classic() + 
    theme(legend.position = "none") + 
    labs(y = NULL, x = NULL) + 
    scale_x_continuous(labels = scales::comma)
}
p_peak_infections <- lapply(unique(peak_infections$R0), plot_peak_infections_facet_by_R0)

p_peak_infections[[1]] <- p_peak_infections[[1]] + theme(strip.background.x = element_blank())
p_peak_infections[[2]] <- p_peak_infections[[2]] + theme(strip.text.x = element_blank())
p_peak_infections[[3]] <- p_peak_infections[[3]] + theme(strip.text.x = element_blank())

p_peak_infections <- cowplot::plot_grid(plotlist = p_peak_infections,
                   nrow = 3) # could adjust rel height to equal size eventually

title <- cowplot::ggdraw() + 
  cowplot::draw_label("Peak infections", fontface='bold', x = 0, hjust = 0, size=18) 
x_title <- cowplot::ggdraw() + 
  cowplot::draw_label("Introduction location", x = 0.5, hjust = 0.5, size=15)
y_title <- cowplot::ggdraw() + 
  cowplot::draw_label(expression(R[0]), y = 0.5, vjust = 0.5, size=15, angle = -90)
p_peak_infections_y_title <- cowplot::plot_grid(p_peak_infections, y_title,
                                              rel_widths = c(0.9, 0.05),
                                              ncol=2)

p_peak_infections_title <- cowplot::plot_grid(title, x_title, p_peak_infections_y_title,
                                            rel_heights = c(0.05, 0.05, 0.9),
                                            nrow=3)

ggsave("output/figures/peak_infections.png",
       p_peak_infections_title,
       width=10.2, height=6, units='in')

plot_arrival_time_facet_by_R0 <- function(facet_R0){
  arrival_times %>% 
    filter(R0 == facet_R0) %>% 
    group_by(R0, mobility_type, name2) %>% 
    summarise(mean_introduction_date_mean = mean(mean_introduction_date),
              mean_introduction_date_min = min(mean_introduction_date),
              mean_introduction_date_0.25 = quantile(mean_introduction_date, 0.25),
              mean_introduction_date_0.75 = quantile(mean_introduction_date, 0.75),
              mean_introduction_date_max = max(mean_introduction_date)) %>% 
    ggplot() + 
    geom_segment(aes(x = mean_introduction_date_min, xend=mean_introduction_date_max, y=mobility_type, yend=mobility_type,
                     color=mobility_type),
                 size=3, alpha=0.4) + 
    geom_segment(aes(x = mean_introduction_date_0.25, xend=mean_introduction_date_0.75, y=mobility_type, yend=mobility_type,
                     color=mobility_type),
                 size=3, alpha=0.4) + 
    geom_point(aes(x = mean_introduction_date_mean, y=mobility_type,
                   color=mobility_type),
               size=0.3) + 
    facet_grid( R0 ~ name2) + 
    theme_classic() + 
    theme(legend.position = "none") + 
    labs(y = NULL, x = NULL) + 
    scale_x_continuous(labels = scales::comma)
}

p_arrival_times <- lapply(unique(arrival_times$R0), plot_arrival_time_facet_by_R0)

p_arrival_times[[1]] <- p_arrival_times[[1]] + theme(strip.background.x = element_blank())
p_arrival_times[[2]] <- p_arrival_times[[2]] + theme(strip.text.x = element_blank())
p_arrival_times[[3]] <- p_arrival_times[[3]] + theme(strip.text.x = element_blank())

p_arrival_times <- cowplot::plot_grid(plotlist = p_arrival_times,
                                        nrow = 3) # could adjust rel height to equal size eventually
title <- cowplot::ggdraw() + 
  cowplot::draw_label("Average time of epidemic introduction", 
                      fontface='bold', x = 0, hjust = 0, size=18)
x_title <- cowplot::ggdraw() + 
  cowplot::draw_label("Introduction location", x = 0.5, hjust = 0.5, size=15)
y_title <- cowplot::ggdraw() + 
  cowplot::draw_label(expression(R[0]), y = 0.5, vjust = 0.5, size=15, angle = -90)

p_arrival_times_y_title <- cowplot::plot_grid(p_arrival_times, y_title,
                                                rel_widths = c(0.9, 0.05),
                                                ncol=2)

p_arrival_times_title <- cowplot::plot_grid(title, x_title, p_arrival_times_y_title,
                                              rel_heights = c(0.05, 0.05, 0.9),
                                              nrow=3)

ggsave("output/figures/arrival_times.png",
       p_arrival_times_title,
       width=10.2, height=6, units='in')

population_data <- data.frame(population=population,
                              pcod=names(pcods))

a2_cent <- a2 %>% 
  filter(pcod %in% peak_infections$introduction_location) %>% 
  st_centroid() %>% 
  mutate(y = unlist(map(geometry,2)),
         x = unlist(map(geometry,1))) %>% 
  left_join(population_data, by=c("pcod")) %>% 
  mutate(name2_label = paste0(name2, " (", scales::comma(round(population, -3)), ")"))

introduction_map <- a2 %>% 
  ggplot() + 
  geom_sf(aes(fill=pcod%in%peak_infections$introduction_location),
          size=0.1) + 
  scale_fill_manual(values=c("TRUE"="blue", "FALSE"="grey")) + 
  ggrepel::geom_label_repel(data=a2_cent, aes(x = x, y=y, label=name2_label),
                            segment.size  = 0.3,
                            nudge_x = c(1, 0, 1, -0.2, 0),
                            nudge_y = c(0.5, 0.5, 0, 0.5, 0.5),
             size=2) + 
  theme_void() + 
  theme(legend.position = "none") 

ggsave("output/figures/introduction_map.png",
       introduction_map,
       width=4, height=6, units='in')

