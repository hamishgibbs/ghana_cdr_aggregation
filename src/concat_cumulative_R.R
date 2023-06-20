suppressPackageStartupMessages({
  require(data.table)
})

if (interactive()){
  .args <- c(
    list.files("data/epi_modelling/results_slim/focus", 
               pattern='gravity_exp.all_pairs.R0_3.infected_fid009',
               full.names = T),
    "data/epi_modelling/results/R_cumulative_quantiles/gravity_exp.all_pairs.R0_3.infected_fid009.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

get_R_trajectories <- function(fn){
  cumsum(c(0, diff(as.numeric(readr::read_rds(fn)$national_epi$R))))
}

R_samples <- do.call(cbind, lapply(.args[1:(length(.args)-1)], get_R_trajectories))

run_metadata <- readr::read_rds(.args[1])$national_epi[, c("time", "mobility_model_type", "mobility_network_type", "introduction_location", "R0")]

my_quantiles <- data.frame(t(apply(R_samples, 1, quantile, probs = c(0.05, 0.2, 0.4, 0.5, 0.6, 0.8, 0.95), na.rm=T)))

R_cumulative_quantiles <- cbind(run_metadata, my_quantiles)

fwrite(R_cumulative_quantiles, tail(.args, 1))
