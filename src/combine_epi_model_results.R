suppressPackageStartupMessages({
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    list.files("data/epi_modelling/results/gravity_exp/all_pairs/R0_3",
               pattern="trajectory_10.rds", full.names = T),
    ""
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

trajectories <- list()

trajectory_indices <- c(1:(length(.args) -1))

for (i in 1:length(trajectory_indices)){
  trajectories[[i]] <- read_rds(.args[trajectory_indices[i]]) %>%
    select(node, time, I, mobility_network_type, R0, introduction_location, sample)
  print(paste0("Progress (reading): ", scales::percent(i / length(trajectory_indices))))
}

trajectories <- do.call(rbind, trajectories)

write_csv(trajectories, tail(.args, 1))

trajectory_peaks <- trajectories %>% 
  group_by(mobility_network_type, R0, introduction_location, sample) %>% 
  top_n(1, wt=I)
/?
  \

write_csv(trajectory_peaks, gsub(".rds", "_peaks.rds", tail(.args, 1)))

