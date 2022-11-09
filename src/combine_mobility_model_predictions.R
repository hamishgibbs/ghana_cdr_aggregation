suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(cowplot)
})

if(interactive()){
  .args <-  c("data/distance/distance_matrix_admin2.rds",
              "data/networks/all_pairs_admin2.csv",
              "data/networks/sequential_admin2.csv",
              "data/mobility_modelling/gravity_exp/all_pairs_model_predictions.rds",
              "data/mobility_modelling/gravity_exp/sequential_model_predictions.rds",
              "data/mobility_modelling/gravity_power/all_pairs_model_predictions.rds",
              "data/mobility_modelling/gravity_power/sequential_model_predictions.rds",
              "data/mobility_modelling/radiation_basic/all_pairs_model_predictions.rds",
              "data/mobility_modelling/radiation_basic/sequential_model_predictions.rds",
              "")
} else {
  .args <- commandArgs(trailingOnly = T)
}

distance_matrix <- read_rds(.args[1])

parse_model_params <- function(fn){
  if (grepl("all_pairs", fn)){
    network_type <- "all_pairs"
  } else {
    network_type <- "sequential"
  }
  return(list(
    model = tail(stringr::str_split(fn, "/")[[1]], 2)[1],
    network_type = network_type,
    fn = fn
  ))
}

matrix_to_long_df_to_plot <- function(m){
  df <- data.frame(cbind(pcod_from = rownames(m), m))
  
  return(df %>%
           pivot_longer(!c(pcod_from), names_to = "pcod_to", values_to = "value") %>%
           drop_na(value) %>%
           mutate(value=as.numeric(value),
                  value = as.integer(value)) %>%
           # Removes internal population
           filter(pcod_from != pcod_to,
                  value > 0))
  
}

read_modelled_network <- function(fn){
  params <- parse_model_params(fn)
  data <- matrix_to_long_df_to_plot(read_rds(fn))
  data$model <- params$model
  data$network_type <- params$network_type
  return (data)
}

all_pairs <- read_csv(.args[2], col_types = cols()) %>% select(-value_sum) %>% 
  mutate(model = "empirical", network_type = "all_pairs") %>% 
  rename(value = value_mean)
sequential <- read_csv(.args[3], col_types = cols()) %>% select(-value_sum) %>% 
  mutate(model = "empirical", network_type = "sequential") %>% 
  rename(value = value_mean)

modelled_networks <- lapply(.args[4:9], read_modelled_network) %>% 
  do.call(rbind, .)

kernels <- rbind(all_pairs, sequential, modelled_networks)

kernels$distance <- distance_matrix[cbind(kernels$pcod_from, kernels$pcod_to)]

write_csv(kernels, tail(.args, 1))
