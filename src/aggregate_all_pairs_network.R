require(tidyverse)

all_pairs_files <- list.files("/Users/hamishgibbs/Filr/My Files/Projects/Ghana/movement/update_09_2021/home/flowkit/playground/data/sensitive/aggregates")

sequential_data <- read_csv("/Users/hamishgibbs/Filr/My Files/Projects/Ghana/movement/consecutive_trips/data/consecutive_trips_od_matrix_admin.csv")
  
all_pairs_files