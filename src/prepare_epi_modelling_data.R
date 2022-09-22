suppressPackageStartupMessages({
  require(tidyverse)
})

# could only do this for one type of network at a time

if (interactive()){
  .args <- c(
    "data/population/population_admin2.csv",
    "data/mobility_modelling/gravity_basic/all_pairs_model_predictions.rds",
    ""
  )
  N_MODEL_DATES <- 1000
} else {
  .args <- commandArgs(trailingOnly = T)
  N_MODEL_DATES <- as.numeric(Sys.getenv("N_MODEL_DATES"))
}

pop <- read_csv(.args[1], col_types = cols()) %>% 
  mutate(population = as.integer(population))

network <- reshape2::melt(read_rds(.args[2])) %>% as_tibble()
colnames(network) <- c("node", "dest", "n")

# recode locations as integers
pcod2_names <- pop$pcod2 %>% unique()
recoded_pcod2 <- 1:length(pcod2_names)
names(recoded_pcod2) <- pcod2_names

# recode population
pop$node <- recode(pop$pcod2, !!!recoded_pcod2)

# recode movement networks
network$node <- recode(network$node, !!!recoded_pcod2)
network$dest <- recode(network$dest, !!!recoded_pcod2)
network$event = "extTrans"
network$proportion = 0
network$select = 1
network$shift = 0

daily_events <- list()
for (i in 1:N_MODEL_DATES){
  network$time <- i
  daily_events[[i]] <- network
}

daily_events <- do.call(rbind, daily_events)

write_rds(daily_events, tail(.args, 1))
write_rds(pop$population, "data/epi_modelling/population.rds")
write_rds(recoded_pcod2, "data/epi_modelling/recoded_pcod.rds")

