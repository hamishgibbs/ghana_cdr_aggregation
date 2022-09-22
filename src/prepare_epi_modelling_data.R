suppressPackageStartupMessages({
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    "data/population/population_admin2.csv",
    "data/mobility_modelling/gravity_power/sequential_model_predictions.rds",
    "data/epi_modelling/all_pairs_events.rds"
  )
  N_MODEL_DATES <- 100
} else {
  .args <- commandArgs(trailingOnly = T)
  N_MODEL_DATES <- as.numeric(Sys.getenv("N_MODEL_DATES"))
}

pop <- read_csv(.args[1], col_types = cols()) %>% 
  mutate(population = as.integer(population))

network <- reshape2::melt(read_rds(.args[2])) %>% as_tibble()
colnames(network) <- c("node", "dest", "n")

normalize_movement_network <- function(network){
  # Calculate the average travel from A to B and B to A so that populations 
  # do not monotonically deplete (or increase) over time
  
  return(network %>%
           filter(node != dest) %>% 
           group_by(row_number()) %>% 
           mutate(journey = paste(sort(c(node, dest)), collapse = " --- "),
                  inv_journey = paste(rev(sort(c(node, dest))), collapse = " --- ")) %>% 
           group_by(journey, inv_journey) %>% 
           summarise(n = sum(n) / 2, .groups="drop") %>% 
           pivot_longer(c(journey, inv_journey), values_to = "journey") %>% 
           separate(journey, c("node", "dest"), sep = " --- ") %>% 
           select(-name) %>% 
           mutate(node = as.integer(node),
                  dest = as.integer(dest),
                  n = as.integer(n)) %>% 
           select(node, dest, n))
}

# recode locations as integers
pcod2_names <- pop$pcod2 %>% unique()
recoded_pcod2 <- 1:length(pcod2_names)
names(recoded_pcod2) <- pcod2_names

# recode population
pop$node <- recode(pop$pcod2, !!!recoded_pcod2)

# recode movement networks
network$node <- recode(network$node, !!!recoded_pcod2)
network$dest <- recode(network$dest, !!!recoded_pcod2)
network <- normalize_movement_network(network)
network$event = "extTrans"
network$proportion = as.integer(0)
network$select = as.integer(1)
network$shift = as.integer(0)
network$n = as.integer(round(network$n, 0))

daily_events <- list()
for (i in 1:N_MODEL_DATES){
  network$time <- i
  daily_events[[i]] <- network
}

daily_events <- do.call(rbind, daily_events)

write_rds(daily_events, tail(.args, 1))
write_rds(pop, "data/epi_modelling/population.rds")
write_rds(recoded_pcod2, "data/epi_modelling/recoded_pcod.rds")
