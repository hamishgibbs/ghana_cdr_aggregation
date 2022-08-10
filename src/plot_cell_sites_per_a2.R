suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
})

source("src/utils/mobility_type_scales.R")

if (interactive()){
  .args <- c(
    "data/cell_sites/cell_sites_admin2.csv",
    "data/geo/admin2.geojson",
    "data/population/population_admin2.csv",
    "output/figures/cell_sites_per_district.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

cell_sites <- read_csv(.args[1])

admin2 <- st_read(.args[2]) %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  st_simplify(preserveTopology = T, dTolerance=100)

population <- read_csv(.args[3])

p_map <- admin2 %>% 
  left_join(cell_sites, by = c("pcod"="pcod2")) %>% 
  ggplot() + 
  geom_sf(aes(fill = n_cell_sites), size=0) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill="Number of\ncell sites") + 
  theme(legend.position = c(0.98, 0.78))
  
ggsave(tail(.args, 1),
       p_map,
       width=5.5, height=7, units="in")

p_cell_sites_population <- cell_sites %>% 
  left_join(population, by="pcod2") %>% 
  ggplot() + 
  geom_jitter(aes(x = population, y = n_cell_sites), size=0.5) + 
  theme_classic() + 
  labs(y="Number of cell sites", x = "Population") + 
  scale_x_continuous(labels = scales::comma)

ggsave(gsub(".png", "_population.png", tail(.args, 1)),
       p_cell_sites_population,
       width=10, height=5.5, units="in")
