require(tidyverse)

admin <- read_csv("/Users/hamishgibbs/Filr/My Files/Projects/Ghana/movement/consecutive_trips/data/consecutive_trips_od_matrix_admin.csv")

admin$dt %>% unique() %>% sort()

ll <- read_csv("/Users/hamishgibbs/Filr/My Files/Projects/Ghana/movement/consecutive_trips/data/consecutive_trips_lon-lat.csv")

new_dates <- ll$dt %>% unique() %>% sort()

old_path <- "/Users/hamishgibbs/Filr/My Files/Projects/Ghana/movement/update_09_2021/home/flowkit/playground/data/sensitive/aggregates"
files <- list.files(old_path)

file_types <- sapply(str_split(files, "[.]"), "[[", 1) %>% unique()
dates <- sapply(str_split(files, "[.]"), "[[", 2)
old_dates <- as.Date(dates) %>% unique() %>% sort()

overlapping_days <- intersect(as.character(new_dates), as.character(old_dates))

# Check density of networks on the same day - asserting that all pairs network is more dense than the non-all pairs network

reference_date <- overlapping_days[1]
test_fn <- paste0("/trips_per_day_admin2.", reference_date, "T00:00:00.csv")

all_pairs <- read_csv(paste0(old_path, test_fn)) %>%
  filter(pcod_from != pcod_to)
sequential <- admin %>% filter(dt == reference_date,
                              admin_level == 2,
                              pcod_from != pcod_to)

# Checking that the "admin" data is admin 2 - yes it is
intersect(sequential$pcod_from %>% unique(), all_pairs$pcod_to %>% unique())

all_pairs
sequential

get_edge_number <- function(network){
  return(
    network %>% mutate(edge = paste0(pcod_from, pcod_to)) %>%
      pull(edge) %>% unique() %>% length())
}
get_trip_number <- function(network){
  return(
    network %>% pull(value) %>% sum()
  )
}

# Which netork has more unique trips?
get_edge_number(all_pairs)
get_edge_number(sequential)
# all pairs has more edges which is what we would expect because there is more tracking of individual people

# Which network has more recorded travellers?
get_trip_number(all_pairs) %>% scales::comma()
get_trip_number(sequential) %>% scales::comma()
# ... this is completely backwards of what is expected? - no

(get_trip_number(all_pairs) - get_trip_number(sequential)) / get_trip_number(all_pairs)

# this validates the basic assumption
