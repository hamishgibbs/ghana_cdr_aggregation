suppressPackageStartupMessages({
  require(tidyverse)
  require(SimInf)
})

if (interactive()){
  .args <- c(
    "src/seir_model.R",
    "data/epi_modelling/population.rds",
    "data/epi_modelling/intro_locs_focus.csv",
    "data/epi_modelling/events/gravity_exp/all_pairs_events.rds",
    "data/epi_modelling/events/gravity_exp/sequential_events.rds",
    "data/epi_modelling/events/gravity_power/all_pairs_events.rds",
    "data/epi_modelling/events/gravity_power/sequential_events.rds",
    "data/epi_modelling/events/radiation_basic/all_pairs_events.rds",
    "data/epi_modelling/events/radiation_basic/sequential_events.rds",
    "data/epi_modelling/results/DONE_FOCUS_LOCS.rds"
  )
  N_MODEL_RUNS <- 10
} else {
  .args <- commandArgs(trailingOnly = T)
  N_MODEL_RUNS <- as.numeric(Sys.getenv("N_MODEL_RUNS"))
}

source(.args[1])
population <- read_rds(.args[2])
intro_locations <- read_csv(.args[3], col_types = cols())

model_format_pop <- population$population
names(model_format_pop) <- population$pcod2

permutation_list <- list(
  mobility_model_type = c("gravity_exp", "gravity_power", "gravity_scaled_power", "radiation_basic"),
  mobility_network_type = c("all_pairs", "sequential"),
  r_value = c(3, 1.5, 1.25),
  intro_location = intro_locations$pcod2,
  model_run = 1:N_MODEL_RUNS
)

model_permutations <- do.call(expand.grid, permutation_list)

model_permutations <- model_permutations %>% 
  left_join(intro_locations, by = c("intro_location" = "pcod2"))

model_permutations$output_fn <- paste0("data/epi_modelling/results/", 
       model_permutations$mobility_model_type, "/", 
       model_permutations$mobility_network_type, "/R0_", 
       model_permutations$r_value, "/infected_", 
       model_permutations$intro_location, "_trajectory_", 
       model_permutations$model_run, ".rds")

model_permutations <- model_permutations %>% arrange(mobility_model_type, 
                               mobility_network_type, 
                               r_value,
                               intro_location,
                               model_run)

model_permutations_list <- model_permutations %>% group_by(output_fn) %>% group_split()

print(paste0("Number of permutations: ", scales::comma(length(model_permutations_list))))

events <- list(
  mobility_model_type = "",
  mobility_network_type = "",
  data = NULL
)

for (i in 1:length(model_permutations_list)){
  model_params <- model_permutations_list[[i]]
  
  # load new events data when either the movement model or network type changes
  if (events$mobility_model_type != model_params$mobility_model_type | 
      events$mobility_network_type != model_params$mobility_network_type){
    
    print(paste0("Loading new events: ", model_params$mobility_model_type, ", ", model_params$mobility_network_type))
    
    events$data <- read_rds(paste0("data/epi_modelling/events/", model_params$mobility_model_type, "/", 
                                   model_params$mobility_network_type, "_events.rds"))
    events$mobility_model_type <- model_params$mobility_model_type
    events$mobility_network_type <- model_params$mobility_network_type
  }
  
  # run the seir model only if the output file does not already exist
  if (!file.exists(model_params$output_fn)){
    
    t1 <- Sys.time()
    
    model <- run_seir_model(infected_location=model_params$node,
                            population=model_format_pop,
                            events=events$data,
                            R0=model_params$r_value)
    model_result <- run(model)
    
    model_trajectory <- trajectory(model_result)
    model_trajectory$mobility_model_type <- model_params$mobility_model_type
    model_trajectory$mobility_network_type <- model_params$mobility_network_type
    model_trajectory$introduction_location <- model_params$intro_location
    model_trajectory$R0 <- model_params$r_value
    model_trajectory$sample <- model_params$model_run
    
    write_rds(model_trajectory, model_params$output_fn)
    
    rm(model)
    rm(model_result)
    gc()
    
    t2 <- Sys.time()
    print(paste0("Progress: ", scales::percent(i / (length(model_permutations_list))),
                 " (", round(as.numeric(t2-t1,units="secs"), 0), " seconds)"))
    
  }
}

# dummy makefile target that will only be written when everything is done processing
write_rds(NULL, tail(.args, 1))
