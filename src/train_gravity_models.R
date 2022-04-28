require(tidyverse)
require(mobility)

population <- read_csv("data/population/population_admin2.csv")
distance_matrix <- read_rds("data/distance/distance_matrix_admin2.rds")

all_pairs <- read_csv("data/networks/all_pairs_admin2.csv") 
sequential <- read_csv("data/networks/sequential_admin2.csv")

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
             model='gravity',
             type='power',
             n_chain=4,
             n_burn=100,
             n_samp=2500,
             n_thin=2,
             DIC=TRUE) 
  )
}

all_pairs_model_inputs <- create_model_inputs(population, distance_matrix, all_pairs)
sequential_model_inputs <- create_model_inputs(population, distance_matrix, sequential)

all_pairs_model <- train_gravity_model(all_pairs_model_inputs, seed=1000)
sequential_model <- train_gravity_model(sequential_model_inputs, seed=1000)

write_rds(all_pairs_model, "output/gravity_modelling/all_pairs_model.rds")
write_rds(sequential_model, "output/gravity_modelling/sequential_model.rds")
write_rds(predict(all_pairs_model), "output/gravity_modelling/all_pairs_model_predictions.rds")
write_rds(predict(sequential_model), "output/gravity_modelling/sequential_model_predictions.rds")
