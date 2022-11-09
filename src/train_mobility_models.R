suppressPackageStartupMessages({
  require(tidyverse)
  require(mobility)
})

if (interactive()){
  .args <- c(
    "data/population/population_admin2.csv",
    "data/distance/distance_matrix_admin2.rds",
    "data/networks/all_pairs_admin2.csv",
    ""
  )
  MOBILITY_MODEL <- "gravity"
  MOBILITY_MODEL_TYPE <- "basic"
} else {
  .args <- commandArgs(trailingOnly = T)
  MOBILITY_MODEL <- Sys.getenv("MOBILITY_MODEL")
  MOBILITY_MODEL_TYPE <- Sys.getenv("MOBILITY_MODEL_TYPE")
}

population <- read_csv(.args[1], col_types = cols())
distance_matrix <- read_rds(.args[2])

network <- read_csv(.args[3], col_types = cols())

network_fn_split <- stringr::str_split(.args[3], "/")
MOBILITY_NETWORK_TYPE <- gsub("_admin2.csv", "", network_fn_split[[1]][length(network_fn_split[[1]])])

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

train_gravity_model <- function(inputs, seed=NULL){

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

model <- train_gravity_model(model_inputs, seed=1000)

fn_prefix <- paste0("data/mobility_modelling/", MOBILITY_MODEL, "_", MOBILITY_MODEL_TYPE, "/", MOBILITY_NETWORK_TYPE, "_model")

write_rds(model, paste0(fn_prefix, ".rds"))

write_rds(predict(model), paste0(fn_prefix, "_predictions.rds"))

if (MOBILITY_MODEL == "gravity"){
  write_csv(summary(model), paste0(fn_prefix, "_summary.csv"))
}

png(filename=paste0(fn_prefix, "_check.png"),
    width=2000, height=1500)
check_results <- mobility::check(model)
dev.off()

if (is.null(check_results$DIC)){
  check_results$DIC <- 0
}

write_csv(data.frame(check_results), paste0(fn_prefix, "_check.csv"))
