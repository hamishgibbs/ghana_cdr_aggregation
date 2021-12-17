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

train_gravity_model <- function(inputs){

  return(
    mobility(data=inputs,
             model='gravity',
             type='power',
             n_chain=4,
             n_burn=100,
             n_samp=500,
             n_thin=2,
             DIC=TRUE) 
  )
}

all_pairs_model_inputs <- create_model_inputs(population, distance_matrix, all_pairs)
sequential_model_inputs <- create_model_inputs(population, distance_matrix, sequential)

all_pairs_model <- train_gravity_model(all_pairs_model_inputs)
sequential_model <- train_gravity_model(sequential_model_inputs)

metric <- rownames(summary(all_pairs_model)[1,] %>% t())

rownames(summary(all_pairs_model))

get_model_summary <- function(model){
  return (
    summary(model) %>% 
      as_tibble() %>% round(., 2) %>% 
      mutate(rowname = rownames(summary(model)))
  )
}

display_model_summary <- function(model){
  return (
    get_model_summary(model) %>% 
      mutate(display = paste0(mean, " (", Q2.5, ",", Q97.5, ")")) %>% 
      select(rowname, display)
  )  
}

display_model_summary(all_pairs_model)
display_model_summary(sequential_model)

get_key_difference_metrics <- function(model, postfix) {
  return (
    get_model_summary(model) %>% 
      select(rowname, mean, Q2.5, Q97.5) %>% 
      filter(rowname %in% c("gamma", "omega_1", "omega_2", "theta")) %>% 
      setNames(paste0(names(.), postfix))
  )
}
get_key_difference_metrics(all_pairs_model, "_all_pairs") %>% 
  left_join(get_key_difference_metrics(sequential_model, "_sequential"), 
            by = c("rowname_all_pairs" = "rowname_sequential")) %>% 
  mutate(mean = mean_all_pairs - mean_sequential,
         Q2.5 = Q2.5_all_pairs - Q2.5_sequential,
         Q97.5 = Q97.5_all_pairs - Q97.5_sequential) %>%
  mutate_at(c("mean", "Q2.5", "Q97.5"), round, 2) %>% 
  rename(rowname = rowname_all_pairs) %>% 
  mutate(display = paste0(mean, " (", Q2.5, ",", Q97.5, ")")) %>% 
  select(rowname, display)

all_pairs_summary %>% mutate(metric) %>% pivot_wider(names_from = metric)

combined_summary <- cbind(all_pairs_summary, sequential_summary) %>% 
  round(., 3) %>% as_tibble() %>% 
  mutate(metric)

combined_summary[ , order(names(combined_summary))] %>% view

summary(mod, probs=c(0.025, 0.975), ac_lags=10)
check(mod)

M <- predict(mod)

M
