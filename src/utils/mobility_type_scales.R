mobility_type_pal <- c("#377eb8", "#4daf4a")
names(mobility_type_pal) <- c("all_pairs", "sequential")
mobility_type_labels <- c("All Pairs", "Sequential")
mobility_type_color_scale <- scale_color_manual(values=mobility_type_pal, labels=mobility_type_labels)
mobility_type_color_scale_no_legend <- scale_color_manual(values=mobility_type_pal, labels=mobility_type_labels,
                                                guide="none")
mobility_type_fill_scale <- scale_fill_manual(values=mobility_type_pal, labels=mobility_type_labels)
