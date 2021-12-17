require(tidyverse)

read_csv_with_fn_date <- function(fn){
  fn_date <- str_split(fn, "[.]")[[1]][2]
  data <- read_csv(fn, col_types = cols())
  data$dt <- as.Date(fn_date)
  return(data)
}

aggregate_network_all_times <- function(network){
  return(
    network %>% 
      group_by(pcod_from, pcod_to) %>% 
      summarise(value = sum(value))
  )
}


all_pairs_path <- "/Users/hamishgibbs/Filr/My Files/Projects/Ghana/movement/update_09_2021/home/flowkit/playground/data/sensitive/aggregates/"
all_pairs_files <- list.files(all_pairs_path)
all_pairs_files <- all_pairs_files[grepl("trips_per_day_admin2", all_pairs_files)]
all_pairs_files <- paste0(all_pairs_path, all_pairs_files)

all_pairs <- lapply(all_pairs_files, read_csv_with_fn_date) %>% 
  do.call(rbind, .)

sequential <- read_csv("/Users/hamishgibbs/Filr/My Files/Projects/Ghana/movement/consecutive_trips/data/consecutive_trips_od_matrix_admin.csv")

intersecting_dates <- intersect(as.character(all_pairs$dt), as.character(sequential$dt))

write_rds(length(intersecting_dates), "data/networks/n_intersecting_dates.rds")

all_pairs_prepared <- all_pairs %>% 
  filter(dt %in% as.Date(intersecting_dates))

sequential_prepared <- sequential %>% 
  filter(dt %in% as.Date(intersecting_dates),
         pcod_from != pcod_to,
         admin_level == 2)

all_pairs_prepared <- aggregate_network_all_times(all_pairs_prepared)
sequential_prepared <- aggregate_network_all_times(sequential_prepared)

write_csv(all_pairs_prepared, "data/networks/all_pairs_admin2.csv")
write_csv(sequential_prepared, "data/networks/sequential_admin2.csv")


