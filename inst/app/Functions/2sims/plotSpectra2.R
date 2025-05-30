plotSpectra2 <- function(harvestedprojection, harvestedprojection2, time1, end1, time2, end2) {
  
  data1 <- plotSpectra(harvestedprojection, time_range = time1:end1, return_data = TRUE) %>%
    mutate(sim = "Sim 1")
  data2 <- plotSpectra(harvestedprojection2, time_range = time1:end1, return_data = TRUE) %>%
    mutate(sim = "Sim 2")
  combined_data <- bind_rows(data1, data2)
  
  ggplot(combined_data, aes(x = w, y = value, color = Species, linetype = sim,
                            group = interaction(Species, sim))) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 0.75) +
    labs(x = "Size (g)",
         y = "Biomass Density") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = "bottom") +
    xlim(NA, 10000) +
    scale_x_log10() +
    scale_y_log10() +
    scale_linetype_manual(values = c("Sim 1" = "solid", "Sim 2" = "dashed")) +
    guides(linetype = guide_legend(title = "Simulation"),
           color = guide_legend(title = "Species"))
}
