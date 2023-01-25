suppressPackageStartupMessages({
  require(tidyverse)
})

source("src/utils/mobility_type_scales.R")

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    "data/epi_modelling/results/gravity_power/focus_locs_results_national.csv",
    "data/epi_modelling/results/gravity_exp/focus_locs_results_national.csv",
    "data/epi_modelling/results/radiation_basic/focus_locs_results_national.csv",
    "output/figures/modelled_trajectories_comparison.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

pcods_a2 <- read.csv(.args[1], header = F, col.names = c("pcod", "name2"))

traj_gravity_power <- read_csv(.args[2], col_types=cols())
traj_gravity_power$model <- "Gravity Model (Power)"
traj_gravity_exp <- read_csv(.args[3], col_types=cols())
traj_gravity_exp$model <- "Gravity Model (Exponential)"
traj_gravity_radiation_basic <- read_csv(.args[4], col_types=cols())
traj_gravity_radiation_basic$model <- "Radiation Model"

trajectories <- do.call(rbind, list(
    traj_gravity_power, 
    traj_gravity_exp, 
    traj_gravity_radiation_basic)) %>% 
  filter(introduction_location == "fid029")

trajectories_named <- trajectories %>% 
  left_join(pcods_a2, by=c("introduction_location" = "pcod")) %>% 
  mutate(introduction_location = name2) %>% select(-name2) %>% 
  mutate(R0 = factor(R0, levels=c("3", "1.5", "1.25"), labels=c("R=3", "R=1.5", "R=1.25")))

p_time_series <- trajectories_named %>% 
  ggplot() + 
  geom_path(aes(x = time, y = I, color=mobility_network_type, 
                group=paste0(sample,mobility_network_type,R0,model)), 
            size=0.3) + 
  facet_grid(R0 ~ model, scales="free") + 
  scale_y_continuous(labels = scales::comma) + 
  theme_classic() + 
  theme(strip.background = element_blank()) + 
  labs(y = "Number of infections", x = "Time (Days)",
       color = "Aggregation\nMethodology") + 
  mobility_type_color_scale + 
  guides(color = guide_legend(override.aes = list(size = 3)))

ggsave(tail(.args, 1),
       p_time_series,
       width=10, height=5.5, units="in")  

