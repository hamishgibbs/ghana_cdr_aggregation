suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    "data/geo/admin2.geojson",
    "data/epi_modelling/results/gravity_exp/all_locs_results_national_peaks.csv",
    ""
  )
  MOBILITY_MODEL_TITLE <- "Gravity Model (Exponential)"
  TIME_DIFFERENCE_COLOR_BREAKS <- as.numeric(stringr::str_split("-25,-15,-10,-5,0,5,10,15,25", pattern=",")[[1]])
} else {
  .args <- commandArgs(trailingOnly = T)
  MOBILITY_MODEL_TITLE <- Sys.getenv("MOBILITY_MODEL_TITLE")
  TIME_DIFFERENCE_COLOR_BREAKS <- as.numeric(stringr::str_split(Sys.getenv("TIME_DIFFERENCE_COLOR_BREAKS"), pattern=",")[[1]])
}

a2 <- st_read(.args[1], quiet=T) %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  st_simplify(dTolerance = 100, preserveTopology = T)
peaks <- read_csv(.args[2], col_types = cols())

peaks_time_difference <- peaks %>% 
  select(-sample, -I) %>% 
  group_by(R0, mobility_network_type, introduction_location) %>% 
  top_n(1, wt = time) %>% 
  pivot_wider(names_from = mobility_network_type, values_from=time) %>% 
  mutate(time_difference = all_pairs - sequential)

p_breaks <- peaks_time_difference %>% 
  ggplot() + 
  geom_density(aes(x = time_difference)) + 
  geom_vline(data = data.frame(TIME_DIFFERENCE_COLOR_BREAKS), 
             aes(xintercept=TIME_DIFFERENCE_COLOR_BREAKS),
             color="red")

ggsave(gsub("figures/", "figures/validation/", tail(.args, 1)),
       p_breaks,
       width=10, height=5.5, units="in")  

p <- peaks_time_difference %>% 
  select(-all_pairs, -sequential) %>% 
  left_join(a2 %>% select(-centroid), by = c("introduction_location" = "pcod")) %>% 
  mutate(R0 = factor(R0, levels=c("3", "1.5", "1.25"), labels=c("R=3", "R=1.5", "R=1.25"))) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = time_difference), size=0.1, color="black") + 
  scale_fill_steps2(breaks = TIME_DIFFERENCE_COLOR_BREAKS,
                   low="#33a02c", high="#1f78b4", mid="white",
                   guide = guide_legend(reverse = TRUE)) + 
  facet_wrap(~R0) +
  theme_void() + 
  labs(fill="Epidemic\nPeak Delay\n(Days)", title=MOBILITY_MODEL_TITLE)

ggsave(tail(.args, 1),
       p,
       width=10, height=5.5, units="in")  

