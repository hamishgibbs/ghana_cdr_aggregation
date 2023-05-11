suppressPackageStartupMessages({
  require(data.table)
})

if (interactive()){
  .args <- c(
    "data/epi_modelling/results/t_test_epi_quantities.csv",
    "output/fmt_t_test_epi_quantities.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}


t_test_res <- fread(.args[1])

t_test_res <- t_test_res[order(variable)]


ci_precision <- 2
p_value_precision <- 4

t_test_res[, value := paste0(
  round(ci_lower, ci_precision), " to ", 
  round(ci_upper, ci_precision), 
  " (", round(p_value, p_value_precision), ")")]

t_test_res$value[grepl("NA", t_test_res$value)] <- NA
t_test_res$value <- gsub("[(]0[)]", "(<0.0001)", t_test_res$value)

fmt <- list()

for (variable_val in unique(t_test_res$variable)){
  df <- subset(t_test_res, variable == variable_val)
  df_fmt <- dcast(df, R0 ~ mobility_model_type, value.var = "value")
  df_fmt$variable <- variable_val
  fmt[[variable_val]] <- df_fmt
}

fwrite(do.call(rbind, fmt), tail(.args, 1))
