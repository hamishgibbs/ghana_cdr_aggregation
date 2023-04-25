suppressPackageStartupMessages({
  require(tidyverse)
  require(SimInf)
})

if (interactive()){
  .args <- c(
    "data/epi_modelling/results/gravity_exp/all_pairs/R0_3.0/infected_fid029_trajectory_0.rds"
  )
  pref <- ""
} else {
  .args <- commandArgs(trailingOnly = T)
  pref <- "ghana_cdr_aggregation/"
}

model_params_from_fn <- function(fn){
  fn_split <- stringr::str_split(fn, "/")
  list(
    mobility_model_type = fn_split[[1]][4],
    mobility_network_type = fn_split[[1]][5],
    r_value = stringr::str_split(fn_split[[1]][6], "_")[[1]][2],
    infected = stringr::str_split(fn_split[[1]][7], "_")[[1]][2],
    iteration = stringr::str_split(stringr::str_split(fn_split[[1]][7], "_")[[1]][4], "[.]")[[1]][1]
  )
}

model_params <- model_params_from_fn(tail(.args, 1))

source(paste0(pref, "src/seir_model.R"))
population <- read_rds(paste0(pref, "data/epi_modelling/population.rds"))
events <- read_rds(paste0(pref, "data/epi_modelling/events/", model_params$mobility_model_type, "/", model_params$mobility_network_type, "_events.rds"))

model_format_pop <- population$population
names(model_format_pop) <- population$pcod2

model <- run_seir_model(infected_location=subset(population, pcod2 == model_params$infected)$node,
                        population=model_format_pop,
                        events=events,
                        R0=as.numeric(model_params$r_value))

model_result <- run(model)

model_trajectory <- trajectory(model_result)
model_trajectory$mobility_model_type <- model_params$mobility_model_type
model_trajectory$mobility_network_type <- model_params$mobility_network_type
model_trajectory$introduction_location <- model_params$infected
model_trajectory$R0 <- model_params$r_value
model_trajectory$sample <- model_params$iteration

write_rds(model_trajectory, paste0(pref, .args[1]))
