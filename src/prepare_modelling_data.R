require(tidyverse)

pop <- read_csv("data/population/population_admin2.csv") %>% 
  mutate(population = as.integer(population)) #%>% 
  #slice(1:100)

all_pairs <- read_csv("data/networks/all_pairs_admin2.csv")
sequential <- read_csv("data/networks/sequential_admin2.csv")

n_dates <- read_rds("data/networks/n_intersecting_dates.rds")

# recode locations as integers
pcod2_names <- pop$pcod2 %>% unique()
recoded_pcod2 <- 1:length(pcod2_names)
names(recoded_pcod2) <- pcod2_names

# recode movement networks
all_pairs$pcod_from <- recode(all_pairs$pcod_from, !!!recoded_pcod2)
all_pairs$pcod_to <- recode(all_pairs$pcod_to, !!!recoded_pcod2)
sequential$pcod_from <- recode(sequential$pcod_from, !!!recoded_pcod2)
sequential$pcod_to <- recode(sequential$pcod_to, !!!recoded_pcod2)

all_pairs <- all_pairs %>% 
  drop_na(pcod_from, pcod_to)

sequential <- sequential %>% 
  drop_na(pcod_from, pcod_to)

# recode population
pop$pcod2 <- recode(pop$pcod2, !!!recode)

create_daily_movement <- function(network, n_dates){
  res <- list()
  for (i in 1:n_dates){
    print(i)
    day_net <- network %>% 
      group_by(row_number()) %>% 
      mutate(journey = paste(sort(c(pcod_from, pcod_to)), collapse = " --- "),
             inv_journey = paste(rev(sort(c(pcod_from, pcod_to))), collapse = " --- ")) %>% 
      group_by(journey, inv_journey) %>% 
      summarise(value_mean = sum(value_mean) / 2, .groups="drop") %>% 
      pivot_longer(c(journey, inv_journey), values_to = "journey") %>% 
      separate(journey, c("pcod_from", "pcod_to"), sep = " --- ") %>% 
      select(-name) %>% 
      rename(n=value_mean,
             node=pcod_from,
             dest=pcod_to)
    
    #day_net <- network %>% 
    #  select(-value_sum) %>% 
    #  left_join(pop, by = c("pcod_from" = "pcod2")) %>% 
    #  mutate(proportion = value_mean / population) %>% 
    #  rename(node=pcod_from,
    #         dest=pcod_to)
    day_net$time = i
    day_net$event = "extTrans"
    day_net$n = 0
    # DEV: Use the fourth column in the select matrix where all compartments can 
    # be sampled with equal weight.
    # DEV: we could add compartment-specific propensity to travel 
    # DEV: check this
    day_net$select = 1
    day_net$shift = 0
    
    res[[i]] <- day_net
  }
  return (do.call(rbind, res))
  
}

all_pairs_events <- create_daily_movement(all_pairs, n_dates)
sequential_events <- create_daily_movement(sequential, n_dates)

# Check that new n is in line with value_sum

write_rds(all_pairs_events, "data/modelling/all_pairs_events.rds")
write_rds(sequential_events, "data/modelling/sequential_events.rds")
write_rds(pop$population, "data/modelling/population.rds")
write_rds(length(recode), "data/modelling/n_patches.rds")
