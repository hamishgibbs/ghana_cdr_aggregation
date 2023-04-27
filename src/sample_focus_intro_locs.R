suppressPackageStartupMessages({
    require(data.table)
    require(sf)
})

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    "data/geo/admin2.geojson",
    "",
    ""
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 2)

districts <- fread(.args[1], header = F)

a2 <- st_read(.args[2])
a2$geometry <- st_make_valid(a2$geometry)
a2 <- st_simplify(a2, preserveTopology = T, dTolerance=100)

district_names <- districts$V2

heuristic_districts <- c("AMA-OKAIKOI SOUTH", 
                       "KMA-MANHYIA SOUTH", 
                       "TM-TAMALE SOUTH",
                       "LAWRA",
                       "NKWANTA SOUTH")

ex_largets_districts <- district_names[which(!district_names %in% heuristic_districts)]

set.seed(1000)
introduction_locations <- c(heuristic_districts, sample(sort(ex_largets_districts), 15, replace = F))

introduction_pcods <- subset(districts, V2 %in% introduction_locations)$V1

fwrite(data.frame(pcod = introduction_pcods), .outputs[1])

a2$intro <- a2$pcod %in% introduction_pcods

p <- ggplot(data = a2) + 
  geom_sf(aes(fill = intro), size=0.2, color='black') + 
  theme_void() + 
  scale_fill_manual(values = c("#EFEFEF", "red")) + 
  theme(legend.position = "none")

ggsave(.outputs[2],
       p,
       width=10, height=5.5, units="in")
