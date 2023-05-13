suppressPackageStartupMessages({
  library(data.table)
})

x <- data.table(
  n = 1:20
)

x[, all_pairs := (n*(n-1))/2]
x[, sequential := (n-1)]

plot_scaling <- function(n, y){
  ggplot() + 
    geom_path(aes(x = n, y = y),
              color="red",
              size=0.8) + 
    theme_classic() + 
    scale_y_continuous(position = "right") + 
    labs(
      x = NULL,
      y = NULL
    ) + 
    theme(axis.text = element_text(size=13))
}


width <- 6
height <- 3.5

p_seq <- plot_scaling(x$n, x$sequential)

ggsave(
  "output/figures/seq_edge_scaling.png",
  width=width,
  height=height,
  units = "in"
)

p_ap <- plot_scaling(x$n, x$all_pairs)

ggsave(
  "output/figures/ap_edge_scaling.png",
  width=width,
  height=height,
  units = "in"
)

