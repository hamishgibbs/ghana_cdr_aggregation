suppressPackageStartupMessages({
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    list.files("data/epi_modelling/results/focus_locs", 
               pattern=".rds", recursive = T, full.names = T),
    ""
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

trajectories <- list()

trajectory_indices <- c(1:(length(.args) -1))

for (i in 1:length(trajectory_indices)){
  trajectories[[i]] <- read_rds(.args[trajectory_indices[i]]) %>% 
    select(node, time, I, mobility_type, R0, introduction_location, sample)
}

trajectories <- do.call(rbind, trajectories)

write_csv(trajectories, tail(.args, 1))