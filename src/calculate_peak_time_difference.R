suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    "data/epi_modelling/results/gravity_exp/all_locs_results_national_peaks.csv",
    "data/epi_modelling/results/gravity_power/all_locs_results_national_peaks.csv",
    "data/epi_modelling/results/radiation_basic/all_locs_results_national_peaks.csv",
    "data/mobility_modelling/peak_time_differences.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

read_national_peaks_with_model_type <- function(fn){
  model <- tail(stringr::str_split(fn, "/")[[1]], 2)[1]
  peaks <- read_csv(fn, col_types = cols())
  peaks$model <- model
  peaks
}

peaks <- lapply(.args[1:3], read_national_peaks_with_model_type) %>% 
  do.call(rbind, .)

peaks_time_difference <- peaks %>% 
  select(-sample, -I) %>% 
  group_by(model, R0, mobility_network_type, introduction_location) %>% 
  top_n(1, wt = time) %>% 
  pivot_wider(names_from = mobility_network_type, values_from=time) %>% 
  mutate(time_difference = all_pairs - sequential)

write_csv(peaks_time_difference, tail(.args, 1))
