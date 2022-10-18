suppressPackageStartupMessages({
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    "",
    ""
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

all_pairs_files <- .args[grepl("trips_per_day_admin2", .args)]
sequential <- read_csv(.args[length(.args)-1], col_types=cols())

read_csv_with_fn_date <- function(fn){
  fn_date <- str_split(fn, "[.]")[[1]][6]
  data <- read_csv(fn, col_types = cols())
  data$dt <- as.Date(fn_date)
  return(data)
}

aggregate_network_all_times <- function(network, contemporaneous_obs){
  return(
    network %>% 
      left_join(contemporaneous_obs, by=c("pcod_from", "pcod_to", "dt")) %>% 
      filter(contemporaneous) %>% 
      group_by(pcod_from, pcod_to) %>% 
      summarise(n_days = n(),
                value_sum = sum(value),
                value_mean = sum(value) / n_days,
                .groups="drop") %>% 
      select(-n_days)
  )
}

all_pairs <- lapply(all_pairs_files, read_csv_with_fn_date) %>% 
  do.call(rbind, .)

intersecting_dates <- intersect(as.character(all_pairs$dt), as.character(sequential$dt))

write_rds(length(intersecting_dates), "data/networks/n_intersecting_dates.rds")

all_pairs_prepared <- all_pairs %>% 
  filter(dt %in% as.Date(intersecting_dates))

sequential_prepared <- sequential %>% 
  filter(dt %in% as.Date(intersecting_dates),
         pcod_from != pcod_to,
         admin_level == 2)

write_csv(all_pairs_prepared, gsub("all_pairs_admin2.csv", "all_pairs_admin2_timeseries.csv", tail(.args, 1)))
write_csv(sequential_prepared, gsub("all_pairs_admin2.csv", "sequential_admin2_timeseries.csv", tail(.args, 1)))

contemporaneous_obs <- all_pairs_prepared %>%
  left_join(sequential_prepared, by=c("pcod_from", "pcod_to", "dt")) %>% 
  drop_na(value.y) %>% 
  select(pcod_from, pcod_to, dt) %>% 
  mutate(contemporaneous = T)

print(paste0("All pairs observations contemporaneous with sequential: ", scales::percent(nrow(contemporaneous_obs) / nrow(all_pairs))))
print(paste0("Sequential observations contemporaneous with all pairs: ", scales::percent(nrow(contemporaneous_obs) / nrow(sequential))))

all_pairs_prepared <- aggregate_network_all_times(all_pairs_prepared, contemporaneous_obs=contemporaneous_obs)
sequential_prepared <- aggregate_network_all_times(sequential_prepared, contemporaneous_obs=contemporaneous_obs)

empirical_difference <- all_pairs_prepared %>% 
  left_join(sequential_prepared, by=c("pcod_from", "pcod_to")) %>% 
  mutate(perc_difference = ((value_mean.x - value_mean.y) / value_mean.x)*100) %>% 
  drop_na(perc_difference)

write_csv(all_pairs_prepared, tail(.args, 1))
write_csv(sequential_prepared, gsub("all_pairs", "sequential", tail(.args, 1)))
