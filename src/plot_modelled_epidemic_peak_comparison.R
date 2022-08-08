require(tidyverse)

if (interactive()){
  .args <- c(
    list.files("output/modelling/preliminary/all_pairs", full.names = T),
    list.files("output/modelling/preliminary/sequential", full.names = T)
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

epi_trajectories <- lapply(.args[1:(length(.args)-250)], read_rds) %>% 
  do.call(rbind, .)

epi_trajectories %>% 
  group_by(time, mobility_type, introduction_location, R0, sample) %>% 
  summarise(I = sum(I), .groups="drop") %>% 
  ggplot() + 
  geom_path(aes(x = time, y = I, color=mobility_type, group=sample)) + 
  facet_grid(R0 ~ introduction_location, scales="free")

# maybe averages would be better represented in a table