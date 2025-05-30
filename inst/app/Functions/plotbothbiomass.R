plotbothbiomass <- function(sim, sim2, specie = NULL, 
                            start_time, end_time, 
                            start_time2, end_time2,
                            y_ticks = 6, ylim = c(NA, NA), highlight = NULL, ...) {
  if (is.null(specie)) {
    specie <- sim@params@species_params$species
  }

  bm1 <- getBiomass(sim)
  bm1 <- bm1[as.numeric(dimnames(bm1)[[1]]) >= start_time & 
               as.numeric(dimnames(bm1)[[1]]) <= end_time, , drop = FALSE]
  bm1 <- reshape2::melt(bm1)
  names(bm1) <- c("Year", "Species", "Biomass")
  bm1 <- bm1[bm1$Species %in% c("Total", specie), ]
  bm1$Sim <- "sim1"

  bm2 <- getBiomass(sim2)
  bm2 <- bm2[as.numeric(dimnames(bm2)[[1]]) >= start_time2 & 
               as.numeric(dimnames(bm2)[[1]]) <= end_time2, , drop = FALSE]
  bm2 <- reshape2::melt(bm2)
  names(bm2) <- c("Year", "Species", "Biomass")
  bm2 <- bm2[bm2$Species %in% c("Total", specie), ]
  bm2$Sim <- "sim2"

  plot_dat <- rbind(bm1, bm2)
  
  p <- ggplot(plot_dat, aes(x = Year, y = Biomass, color = Species, linetype = Sim)) +
    geom_line() +
    scale_y_continuous(trans = "log10", name = "Biomass [g]") +
    scale_x_continuous(name = "Year") +
    scale_linetype_manual(values = c("sim1" = "solid", "sim2" = "dashed")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )
  
  return(p)
}
