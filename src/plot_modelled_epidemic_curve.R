suppressPackageStartupMessages({
  require(tidyverse)
})

source("src/utils/mobility_type_scales.R")

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    list.files("output/modelling/preliminary/all_pairs", full.names = T),
    list.files("output/modelling/preliminary/sequential", full.names = T),
    "output/figures/modelled_trajectory.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

pcods_a2 <- read.csv(.args[1], header = F, col.names = c("pcod", "name2"))

trajectories <- list()

trajectory_indices <- c(2:(length(.args) -1))

for (i in 1:length(trajectory_indices)){
  trajectories[[i]] <- read_rds(.args[trajectory_indices[i]]) %>% 
    select(time, I, mobility_type, R0, introduction_location, sample) %>% 
    group_by(time, mobility_type, R0, introduction_location, sample) %>% 
    summarise(I = sum(I), .groups="drop")
}

trajectories <- do.call(rbind, trajectories)

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
  group_by(time, mobility_type, R0, introduction_location) %>% 
  summarise(avg = mean(I),
            lower_90 = quantile(I, 0.05),
            upper_90 = quantile(I, 0.95),
            lower_50 = quantile(I, 0.25),
            upper_50 = quantile(I, 0.75),
            .groups="drop")

p_time_series <- trajectories_density %>% 
  ggplot() + 
  geom_ribbon(aes(x = time, ymin=lower_90, ymax=upper_90, fill=mobility_type), alpha=0.2) + 
  geom_ribbon(aes(x = time, ymin=lower_50, ymax=upper_50, fill=mobility_type), alpha=0.2) + 
  geom_path(aes(x = time, y = avg, color=mobility_type), size=0.3) + 
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

