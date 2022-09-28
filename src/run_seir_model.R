suppressPackageStartupMessages({
  require(tidyverse)
  require(SimInf)
})

if (interactive()){
  .args <- c(
    "src/seir_model.R",
    "data/epi_modelling/population.rds",
    "data/epi_modelling/all_pairs_events.rds",
    "data/epi_modelling/intro_locs_focus.csv",
    "data/epi_modelling/results/focus_locs/all_pairs/R0_1.25/infected_fid240_trajectory_10.rds"
  )
  MODEL_R0_VALUE <- 1.25 #1.25, 1.5, 3.0
  N_MODEL_RUNS <- 1
} else {
  .args <- commandArgs(trailingOnly = T)
  MODEL_R0_VALUE <- as.numeric(Sys.getenv("MODEL_R0_VALUE"))
  N_MODEL_RUNS <- as.numeric(Sys.getenv("N_MODEL_RUNS"))
}

source(.args[1])
population <- read_rds(.args[2])
events <- read_rds(.args[3])
intro_locations <- read_csv(.args[4], col_types = cols()) %>% group_by(pcod2) %>% group_split()

network_fn_split <- stringr::str_split(.args[3], "/")
MOBILITY_NETWORK_TYPE <- gsub("_events.rds", "", network_fn_split[[1]][length(network_fn_split[[1]])])

model_format_pop <- population$population
names(model_format_pop) <- population$pcod2

output_fn_split <- stringr::str_split(tail(.args, 1), "/")[[1]]
output_path <- paste(output_fn_split[1:(length(output_fn_split)-1)], collapse="/")

processed_files <- list.files(output_path)
intro_locations_to_process <- list()

# optimization to remove any introduction locations that have 
# already been processed (identifies output fns present N_MODEL_RUNS times)
for (intro_location in intro_locations){
  n_processed <- sum(stringr::str_detect(processed_files, intro_location$pcod2))
  
  if (n_processed != N_MODEL_RUNS){
    intro_locations_to_process[[length(intro_locations_to_process)+1]] <- intro_location 
  }
}

intro_locs_already_processed <- length(intro_locations) - length(intro_locations_to_process)

model_run_number <- 1
for (infected_location in intro_locations_to_process){
  for (sample in 1:N_MODEL_RUNS){
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
    model_trajectory$sample <- sample
    
    write_rds(model_trajectory, paste0(output_path, 
                                       "/infected_", infected_location$pcod2,
                                       "_trajectory_", sample, ".rds"))
    
    t2 <- Sys.time()
    
    files_processed <- (intro_locs_already_processed * N_MODEL_RUNS) + model_run_number
    print(paste0("Progress: ", scales::percent(files_processed / (length(intro_locations)*N_MODEL_RUNS)), 
                 " (", round(as.numeric(t2-t1,units="secs"), 0), " seconds)"))
    model_run_number <- model_run_number + 1
  }
}
