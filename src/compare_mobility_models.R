suppressPackageStartupMessages({
  require(tidyverse)
})

if(interactive()){
  .args <-  c("data/mobility_modelling/gravity_basic/all_pairs_model.rds",
              "data/mobility_modelling/gravity_basic/sequential_model.rds",
              "data/mobility_modelling/gravity_exp/all_pairs_model.rds",
              "data/mobility_modelling/gravity_exp/sequential_model.rds",
              "data/mobility_modelling/gravity_power/all_pairs_model.rds",
              "data/mobility_modelling/gravity_power/sequential_model.rds",
              "")
} else {
  .args <- commandArgs(trailingOnly = T)
}

models <- lapply(.args[1:6], read_rds)

models

mobility::compare(models)
