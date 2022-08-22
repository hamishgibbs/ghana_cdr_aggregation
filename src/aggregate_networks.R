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
sequential <- read_csv(.args[length(.args)-1])

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

write_rds(length(intersecting_dates), "data/networks/n_intersecting_dates.rds")

all_pairs_prepared <- all_pairs %>% 
  filter(dt %in% as.Date(intersecting_dates))

sequential_prepared <- sequential %>% 
  filter(dt %in% as.Date(intersecting_dates),
         pcod_from != pcod_to,
         admin_level == 2)

all_pairs_prepared <- aggregate_network_all_times(all_pairs_prepared)
sequential_prepared <- aggregate_network_all_times(sequential_prepared)

write_csv(all_pairs_prepared, tail(.args, 1))
write_csv(sequential_prepared, gsub("all_pairs", "sequential", tail(.args, 1)))
