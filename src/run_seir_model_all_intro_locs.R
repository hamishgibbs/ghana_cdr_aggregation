suppressPackageStartupMessages({
  require(tidyverse)
  require(SimInf)
})

if (interactive()){
  .args <- c(
    "src/seir_model.R",
    "data/epi_modelling/population.rds",
    "data/epi_modelling/all_pairs_events.rds",
    ""
  )
  MODEL_R0_VALUE <- 3 #1.25, 1.5, 3.0
} else {
  .args <- commandArgs(trailingOnly = T)
  MODEL_R0_VALUE <- as.numeric(Sys.getenv("MODEL_R0_VALUE"))
}

source(.args[1])
population <- read_rds(.args[2])
events <- read_rds(.args[3])

network_fn_split <- stringr::str_split(.args[3], "/")
MOBILITY_NETWORK_TYPE <- gsub("_events.rds", "", network_fn_split[[1]][length(network_fn_split[[1]])])

model_format_pop <- population$population
names(model_format_pop) <- population$pcod2

intro_locations <- population %>% select(-population) %>% group_by(pcod2) %>% group_split()

for (infected_location in intro_locations){
  
  t1 <- Sys.time()
  
  model <- run_seir_model(infected_location=infected_location$node,
                          population=model_format_pop,
                          events=events,
                          R0=MODEL_R0_VALUE)
  
  model_result <- run(model)
  model_trajectory <- trajectory(model_result)
  model_trajectory$mobility_type <- MOBILITY_NETWORK_TYPE
  model_trajectory$introduction_location <- infected_location$pcod2
  model_trajectory$R0 <- MODEL_R0_VALUE
  
  write_rds(model_trajectory, paste0("data/epi_modelling/results/all_intro_locs/", MOBILITY_NETWORK_TYPE, 
                                "/R0_", MODEL_R0_VALUE, 
                                "_infected_", infected_location$pcod2,
                                "_trajectory.rds"))
  
  t2 <- Sys.time()
  
  print(paste0("Progress: ", scales::percent(infected_location$node / length(model_format_pop)), 
               " (", round(as.numeric(t2-t1,units="secs"), 0), " seconds)"))
}
