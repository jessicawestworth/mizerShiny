plotDietCompare <- function(objects, species = NULL,
                            sim_names = NULL) {
  # Helper to build the long data for one sim
  diet_long <- function(obj, idx, species) {
    d <- getDiet(obj@params,
                 n       = apply(obj@n,      2:3, mean),
                 n_pp    = apply(obj@n_pp,   2,   mean),
                 n_other = apply(obj@n_other,2,   mean))
    keep <- dimnames(d)[[1]] %in% species
    d    <- d[keep, , , drop = FALSE]
    names(dimnames(d)) <- c("Predator", "w", "Prey")
    df <- reshape2::melt(d, value.name = "Proportion")
    df <- subset(df, Proportion > 0.001)
    # choose label
    if (!is.null(sim_names) &&
        length(sim_names) >= idx) {
      df$Sim <- sim_names[idx]
    } else {
      df$Sim <- paste0("Sim ", idx)
    }
    df$Prey <- factor(df$Prey,
                      levels = rev(dimnames(d)$Prey))
    df
  }

  # bind all sims
  plot_dat <- dplyr::bind_rows(
    lapply(seq_along(objects),
           function(i) diet_long(objects[[i]], i, species))
  )

  params <- objects[[1]]@params
  legend_levels <- intersect(names(params@linecolour),
                             plot_dat$Prey)

  ggplot2::ggplot(plot_dat,
                  ggplot2::aes(w, Proportion, fill = Prey)) +
    ggplot2::geom_area() +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Size", y = "Proportion") +
    ggplot2::scale_fill_manual(
      values = params@linecolour[legend_levels],
      limits = legend_levels
    ) +
    ggplot2::facet_wrap(~ Sim, nrow = 2) +
    ggplot2::theme_minimal()
}
