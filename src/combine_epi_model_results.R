suppressPackageStartupMessages({
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    list.files("data/epi_modelling/results/gravity_exp/all_pairs/R0_3",
               pattern="trajectory_10.rds", full.names = T),
    "",
    ""
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 2)

trajectories_national <- list()

trajectory_indices <- c(1:(length(.args) -1))

for (i in 1:length(trajectory_indices)){
  trajectories_national[[i]] <- read_rds(.args[trajectory_indices[i]]) %>% 
    select(node, time, I, mobility_network_type, R0, introduction_location, sample) %>% 
    group_by(time, mobility_network_type, R0, introduction_location, sample) %>% 
    summarise(I = sum(I, na.rm = T), .groups="drop")
  
  print(paste0("Progress (reading): ", scales::percent(i / length(trajectory_indices))))
}

trajectories_national <- do.call(rbind, trajectories_national)
  
write_csv(trajectories_national, .outputs[1])

# Note that some epidemics do not finish by the time the end of the model training is reached
trajectory_peaks <- trajectories_national %>%
  group_by(mobility_network_type, R0, introduction_location, sample) %>% 
  top_n(1, wt=I)

write_csv(trajectory_peaks, .outputs[2])

