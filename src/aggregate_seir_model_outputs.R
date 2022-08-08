require(tidyverse)

if (interactive()){
  .args <- c(
    "output/modelling/introduction_pcods.rds"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

introduction_pcods <- read_rds(.args[1])

seir_trajectory_to_epi_peak_per_sample <- function(fn){
  data <- read_rds(fn) %>% 
    group_by(time, R0, mobility_type, introduction_location, sample) %>% 
    summarise(I = sum(I), .groups='drop') %>% 
    group_by(R0, mobility_type, introduction_location, sample) %>% 
    summarise(maxI = max(I), .groups='drop')
  return(data)
}

seir_trajectory_to_arrival_date_per_sample <- function(fn){
  data <- read_rds(fn) %>% 
    group_by(node, R0) %>% 
    mutate(introduction_date = I >= 1 & !duplicated(I >= 1)) %>% 
    filter(introduction_date) %>% 
    group_by(R0, mobility_type, introduction_location, sample) %>% 
    summarise(mean_introduction_date = mean(time),
              median_introduction_date = median(time),
              sd_introduction_date = sd(time), .groups='drop')
  return(data)
}

seir_trajectory_to_duration_per_sample <- function(fn){
  data <- read_rds(fn) %>% 
    filter(I > 0) %>% 
    group_by(node, R0, mobility_type, introduction_location, sample) %>% 
    summarise(duration = max(time) - min(time), .groups="drop") %>% 
    group_by(R0, mobility_type, introduction_location, sample) %>% 
    summarise(mean_duration = mean(duration),
              median_duration = median(duration),
              sd_duration = sd(duration), .groups='drop')
  return(data)
}

all_pairs_fns <- list.files("output/modelling/preliminary/all_pairs",
                            full.names = T)
sequential_fns <- list.files("output/modelling/preliminary/sequential",
                             full.names = T)

all_fns <- c(all_pairs_fns, sequential_fns)

peak_infections <- lapply(all_fns, seir_trajectory_to_epi_peak_per_sample) %>% 
  do.call(rbind, .)

write_csv(peak_infections, "/Users/hamishgibbs/Documents/UCL/ghana_cdr_aggregation/output/modelling/preliminary/peak_infections.csv")

arrival_times <- lapply(all_fns, seir_trajectory_to_arrival_date_per_sample) %>% 
  do.call(rbind, .)

write_csv(arrival_times, "/Users/hamishgibbs/Documents/UCL/ghana_cdr_aggregation/output/modelling/preliminary/arrival_times.csv")

durations <- lapply(all_fns, seir_trajectory_to_duration_per_sample) %>% 
  do.call(rbind, .)

write_csv(durations, "/Users/hamishgibbs/Documents/UCL/ghana_cdr_aggregation/output/modelling/preliminary/durations.csv")

