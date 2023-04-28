suppressPackageStartupMessages({
  require(tidyverse)
  require(SimInf)
})

if (interactive()){
  .args <- c(
    "src/seir_model.R",
    "data/epi_modelling/population.rds",
    "data/epi_modelling/events/gravity_exp/all_pairs_events.rds",
    "data/epi_modelling/results/gravity_exp/all_pairs/R0_3.0/infected_fid029_trajectory_0.rds"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

N_MODEL_DATES <- 2000

source(.args[1])
population <- read_rds(.args[2])
events <- read_rds(.args[3])


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

daily_events <- list()
for (i in seq(from=1, to=N_MODEL_DATES, by=1)){
  events$time <- i
  daily_events[[i]] <- events
}

daily_events <- do.call(rbind, daily_events)

model_format_pop <- population$population
names(model_format_pop) <- population$pcod2

model <- run_seir_model(infected_location=subset(population, pcod2 == model_params$infected)$node,
                        population=model_format_pop,
                        events=daily_events,
                        R0=as.numeric(model_params$r_value))
set_num_threads(1)
model_result <- run(model)

model_trajectory <- trajectory(model_result)
model_trajectory$mobility_model_type <- model_params$mobility_model_type
model_trajectory$mobility_network_type <- model_params$mobility_network_type
model_trajectory$introduction_location <- model_params$infected
model_trajectory$R0 <- model_params$r_value
model_trajectory$sample <- model_params$iteration

write_rds(model_trajectory, tail(.args, 1))
