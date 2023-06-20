suppressPackageStartupMessages({
  require(data.table)
})

if (interactive()){
  .args <- c(
    list.files("data/epi_modelling/results_slim/focus", 
               pattern='gravity_exp.all_pairs.R0_3.infected_fid009',
               full.names = T),
    "data/epi_modelling/results/I_quantiles/gravity_exp.all_pairs.R0_3.infected_fid009.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

get_I_trajectories <- function(fn){
  as.numeric(readr::read_rds(fn)$national_epi$I)
}

I_samples <- do.call(cbind, lapply(.args[1:(length(.args)-1)], get_I_trajectories))

run_metadata <- readr::read_rds(.args[1])$national_epi[, c("time", "mobility_model_type", "mobility_network_type", "introduction_location", "R0")]

my_quantiles <- data.frame(t(apply(I_samples, 1, quantile, probs = c(0.05, 0.2, 0.4, 0.5, 0.6, 0.8, 0.95), na.rm=T)))

I_quantiles <- cbind(run_metadata, my_quantiles)

fwrite(I_quantiles, tail(.args, 1))
