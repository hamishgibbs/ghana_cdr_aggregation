require(sf)
require(tidyverse)
require(raster)

pop <- raster("data/population/gha_ppp_2020.tif")
admin_boundaries <- st_read("data/geo/admin2.geojson") %>% 
  st_simplify(preserveTopology = T, dTolerance=0.0005) %>% 
  #slice(1:10) %>% 
  group_by(pcod) %>% group_split()

aggregated_population <- c()
i <- 0
for (poly in admin_boundaries){
  pop_cropped <- crop(pop, sp::bbox(as_Spatial(poly)))
  aggregated_population <- append(aggregated_population, extract(pop_cropped, poly, fun=sum, na.rm=T))
  i <- i + 1
  print(i)
}

pop_extracted <- tibble(pcod2=as.character(do.call(rbind, admin_boundaries)$pcod),
                        population=round(aggregated_population, 2))

write_csv(pop_extracted, "data/population/population_admin2.csv")

# Sanity
admin_boundaries %>% 
  do.call(rbind, .) %>% 
  left_join(pop_extracted, by=c("pcod" = "pcod2")) %>% 
  ggplot() + 
  geom_sf(aes(fill = population), size = 0) + 
  scale_fill_continuous(trans="log10") + 
  theme_void()

pop_extracted$population %>% sum() %>% scales::comma()


