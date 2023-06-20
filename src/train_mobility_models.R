suppressPackageStartupMessages({
  require(tidyverse)
  require(mobility)
})

if (interactive()){
  .args <- c(
    "data/population/population_admin2.csv",
    "data/distance/distance_matrix_admin2.rds",
    "data/networks/sequential_admin2.csv",
    "data/mobility_modelling/gravity_exp/sequential_model.rds",
    "data/mobility_modelling/gravity_exp/sequential_model_predictions.rds",
    "data/mobility_modelling/gravity_exp/sequential_model_check.png",
    "data/mobility_modelling/gravity_exp/sequential_model_check.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 4)

population <- read_csv(.args[1], col_types = cols())
distance_matrix <- read_rds(.args[2])

network <- read_csv(.args[3], col_types = cols())

MOBILITY_NETWORK_TYPE <- gsub("_model", "", 
                              stringr::str_split(
                                stringr::str_split(
                                  tail(.args, 1), "/")[[1]][4], "[.]")[[1]][1])

model_fn_split <- stringr::str_split(
  stringr::str_split(tail(.args, 1), "/")[[1]][3], "_")[[1]]

MOBILITY_MODEL <- model_fn_split[1]
MOBILITY_MODEL_TYPE <- model_fn_split[2]

create_model_inputs <- function(population, distance, network){

  model_inputs <- list()

  model_inputs$N <- population$population
  names(model_inputs$N) <- population$pcod2

  model_inputs$D <- distance

  model_inputs$M <- reshape2::acast(network, pcod_from~pcod_to, value.var="value_mean")
  model_inputs$M <- model_inputs$M[order(row.names(model_inputs$M)), ]

  model_input_data <- mobility::mobility_matrices

  model_input_data$M
  model_input_data$D
  model_input_data$N

  return(model_inputs)

}

train_mobility_model <- function(inputs, seed=NULL){

  if (seed){set.seed(seed)}
  return(
    mobility(data=inputs,
             model=MOBILITY_MODEL,
             type=MOBILITY_MODEL_TYPE,
             n_chain=4,
             n_burn=10000,
             n_samp=50000,
             n_thin=2,
             DIC=TRUE,
             parallel=TRUE)
  )
}

model_inputs <- create_model_inputs(population, distance_matrix, network)

model <- train_mobility_model(model_inputs, seed=1000)

write_rds(model, .outputs[1])

write_rds(predict(model), .outputs[2])

if (MOBILITY_MODEL == "gravity"){
  write_csv(summary(model), gsub(".rds", "_summary.csv", .outputs[1]))
}

png(filename=.outputs[3],
    width=2000, height=1500)
check_results <- mobility::check(model)
dev.off()

if (is.null(check_results$DIC)){
  check_results$DIC <- 0
}

write_csv(data.frame(check_results), .outputs[4])
