all_pairs_res <- read_rds("output/modelling/preliminary/all_pairs_trajectory.rds")
sequential_res <- read_rds("output/modelling/preliminary/sequential_trajectory.rds")


total_infected_per_time <- function(result){
  return(
    result %>% 
      group_by(time) %>% 
      summarise(I = sum(I))
  )
  
}

sequential_res_total <- total_infected_per_time(sequential_res)
all_pairs_res_total <- total_infected_per_time(all_pairs_res)

ggplot() + 
  geom_path(data = all_pairs_res_total, aes(x = time, y = I), color = "red") + 
  geom_path(data = sequential_res_total, aes(x = time, y = I))

all_pairs_res %>% 
  filter(I == 1000)

all_pairs_res$I
sequential_res$I

which(sequential_res_total$I == max(sequential_res_total$I))
which(all_pairs_res_total$I == max(all_pairs_res_total$I))

# Need to change the training to an MCMC
