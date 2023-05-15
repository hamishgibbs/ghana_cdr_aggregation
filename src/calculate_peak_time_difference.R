suppressPackageStartupMessages({
  require(data.table)
})

if (interactive()){
  .args <- c(
    list.files("data/epi_modelling/results_slim/all", 
               pattern='gravity_exp.all_pairs.R0_3.infected_fid009',
               full.names = T),
    list.files("data/epi_modelling/results_slim/all", 
               pattern='gravity_exp.sequential.R0_3.infected_fid009',
               full.names = T),
    "data/epi_modelling/results/peak_difference/gravity_exp.R0_3.infected_fid009.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

get_national_epi_peak <- function(fn){
  traj <- readr::read_rds(fn)
  which(traj$national_epi$I == max(traj$national_epi$I))[1]
}

peak_ap <- mean(unlist(lapply(.args[1:10], get_national_epi_peak)))
peak_seq <- mean(unlist(lapply(.args[11:20], get_national_epi_peak)))

metadata <- readr::read_rds(.args[1])$national_epi[, c("mobility_model_type", "introduction_location", "R0")]

peak_difference <- unique(metadata)
peak_difference$peak_difference <- peak_seq-peak_ap

fwrite(peak_difference, tail(.args, 1))