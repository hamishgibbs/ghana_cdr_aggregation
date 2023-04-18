suppressPackageStartupMessages({
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    list.files("../../LSHTM/Filr/My_Files/Projects/Ghana/movement/update_09_2021/home/flowkit/playground/data/sensitive/aggregates", 
               pattern="trips_per_day_admin2", full.names = T),
    "../../LSHTM/Filr/My_Files/Projects/Ghana/movement/consecutive_trips/data/consecutive_trips_od_matrix_admin.csv",
    "data/networks/n_intersecting_dates.rds",
    "data/networks/all_pairs_admin2_timeseries.csv",
    "data/networks/sequential_admin2_timeseries.csv",
    "data/networks/all_pairs_admin2.csv",
    "data/networks/sequential_admin2.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 5)

all_pairs_files <- .args[grepl("trips_per_day_admin2", .args)]
sequential <- read_csv(.args[length(.args)-length(.outputs)], col_types=cols())

read_csv_with_fn_date <- function(fn){
  fn_date <- str_split(fn, "[.]")[[1]][6]
  data <- read_csv(fn, col_types = cols())
  data$dt <- as.Date(fn_date)
  return(data)
}

aggregate_network_all_times <- function(network){
  return(
    network %>% 
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

write_rds(length(intersecting_dates), .outputs[1])

all_pairs_prepared <- all_pairs %>% 
  filter(dt %in% as.Date(intersecting_dates))

sequential_prepared <- sequential %>% 
  filter(dt %in% as.Date(intersecting_dates),
         pcod_from != pcod_to,
         admin_level == 2)

write_csv(all_pairs_prepared, .outputs[2])
write_csv(sequential_prepared, .outputs[3])

all_pairs_prepared <- aggregate_network_all_times(all_pairs_prepared)
sequential_prepared <- aggregate_network_all_times(sequential_prepared)

empirical_difference <- all_pairs_prepared %>% 
  left_join(sequential_prepared, by=c("pcod_from", "pcod_to")) %>% 
  mutate(perc_difference = ((value_mean.x - value_mean.y) / value_mean.x)*100) %>% 
  drop_na(perc_difference)

write_csv(all_pairs_prepared, .outputs[4])
write_csv(sequential_prepared, .outputs[5])
