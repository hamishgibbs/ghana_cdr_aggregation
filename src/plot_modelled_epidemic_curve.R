suppressPackageStartupMessages({
  require(tidyverse)
})

source("src/utils/mobility_type_scales.R")

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    "data/epi_modelling/results/gravity_exp/focus_locs_results_national.csv",
    "output/figures/gravity_exp_modelled_trajectory.png"
  )
  MOBILITY_MODEL_TITLE <- "Gravity Model (Exponential)"
} else {
  .args <- commandArgs(trailingOnly = T)
  MOBILITY_MODEL_TITLE <- Sys.getenv("MOBILITY_MODEL_TITLE")
}

pcods_a2 <- read.csv(.args[1], header = F, col.names = c("pcod", "name2"))

trajectories <- read_csv(.args[2], col_types=cols())

trajectories_named <- trajectories %>% 
  left_join(pcods_a2, by=c("introduction_location" = "pcod")) %>% 
  mutate(introduction_location = name2) %>% select(-name2) %>% 
  mutate(introduction_location = factor(introduction_location, 
                                        levels = c("AMA-OKAIKOI SOUTH", 
                                                   "KMA-MANHYIA SOUTH", 
                                                   "TM-TAMALE SOUTH",
                                                   "LAWRA",
                                                   "NKWANTA SOUTH")),
         R0 = factor(R0, levels=c("3", "1.5", "1.25"), labels=c("R=3", "R=1.5", "R=1.25")))

trajectories_density <- trajectories_named %>% 
  group_by(time, mobility_network_type, R0, introduction_location) %>% 
  summarise(med = median(I),
            lower_95 = quantile(I, 0.025),
            upper_95 = quantile(I, 0.975),
            lower_50 = quantile(I, 0.25),
            upper_50 = quantile(I, 0.75),
            .groups="drop")

p_time_series <- trajectories_density %>% 
  ggplot() + 
  geom_ribbon(aes(x = time, ymin=lower_95, ymax=upper_95, fill=mobility_network_type), 
              alpha=0.2) + 
  geom_ribbon(aes(x = time, ymin=lower_50, ymax=upper_50, fill=mobility_network_type), 
              alpha=0.2) + 
  geom_path(aes(x = time, y = med, color=mobility_network_type), size=0.3) + 
  facet_grid(R0 ~ introduction_location, scales="free") + 
  scale_y_continuous(labels = scales::comma) + 
  theme_classic() + 
  theme(legend.position = "none",
        strip.background = element_blank()) + 
  labs(y = "Number of infections", x = "Time") + 
  mobility_type_color_scale + 
  mobility_type_fill_scale

ggsave(tail(.args, 1),
       p_time_series,
       width=10, height=5.5, units="in")  

peaks <- trajectories_named %>% 
  group_by(mobility_network_type, R0, introduction_location, sample) %>% 
  top_n(1, wt=I)

peaks_by_network <- peaks %>% 
  group_by(mobility_network_type, R0, introduction_location) %>% 
  summarise(median_time = median(time),
         median_I = median(I),
         .groups="drop")

peak_by_network_difference <- peaks_by_network %>% 
  pivot_wider(names_from = mobility_network_type, 
              values_from = !c(R0, mobility_network_type, introduction_location)) %>% 
  mutate(median_time_difference = median_time_sequential - median_time_all_pairs,
         median_I_difference = median_I_sequential - median_I_all_pairs)
  
p_difference <- peak_by_network_difference %>% 
  select(R0, introduction_location, median_time_difference, median_I_difference) %>% 
  pivot_longer(!c(R0, introduction_location)) %>% 
  mutate(name = factor(name, levels = c("median_time_difference", "median_I_difference"),
                       labels = c("Timing of epidemic peak", "Peak number of infections"))) %>%  
  ggplot() + 
  geom_bar(aes(x = R0, y = value), stat="identity") + 
  facet_grid(name ~ introduction_location, scales="free_y") + 
  labs(y = "Median difference between networks",
       title = MOBILITY_MODEL_TITLE,
       x = NULL) + 
  theme_classic()

ggsave(gsub("modelled_trajectory.png", "peak_difference.png", tail(.args, 1)),
       p_difference,
       width=10, height=5.5, units="in")
