process_sim <- function(harvestedprojection, unharvestedprojection, year1, year2) {

  unharvestedbio <- getBiomass(unharvestedprojection)[year1:year2, , drop = FALSE] %>%
    melt() %>%
    group_by(sp) %>%
    summarise(value = mean(value, na.rm = TRUE))

  harvestedbio <- getBiomass(harvestedprojection)[year1:year2, , drop = FALSE] %>%
    melt() %>%
    group_by(sp) %>%
    summarise(value = mean(value, na.rm = TRUE))

  percentage_diff_chosen <- left_join(harvestedbio, unharvestedbio, by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% "Resource") %>%
    mutate(class = "chosen")

  calc_biomass_triples <- function(projection) {
    biotriple <- getBiomass(projection)
    low  <- biotriple[max(1, ceiling(year1 * 0.5)):max(2, ceiling(year2 * 0.5)), , drop = FALSE] %>%
      melt() %>%
      group_by(sp) %>%
      summarise(value = mean(value, na.rm = TRUE))
    high <- biotriple[(year1 * 2):(year2 * 2), , drop = FALSE] %>%
      melt() %>%
      group_by(sp) %>%
      summarise(value = mean(value, na.rm = TRUE))
    list(low, high)
  }

  triples_harv   <- calc_biomass_triples(harvestedprojection)
  triples_unharv <- calc_biomass_triples(unharvestedprojection)

  percentage_diff_short <- left_join(triples_harv[[1]], triples_unharv[[1]], by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% "Resource") %>%
    mutate(class = "short")

  percentage_diff_long <- left_join(triples_harv[[2]], triples_unharv[[2]], by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% "Resource") %>%
    mutate(class = "long")

  plot_data <- bind_rows(percentage_diff_short, percentage_diff_chosen, percentage_diff_long)

  plot_data$class <- factor(plot_data$class, levels = c("short", "chosen", "long"))
  plot_data$fill_group <- interaction(plot_data$percentage_diff >= 0, plot_data$class)
  plot_data$fill_group <- factor(
    plot_data$fill_group,
    levels = c("FALSE.short", "TRUE.short", "FALSE.chosen", "TRUE.chosen", "FALSE.long", "TRUE.long"),
    labels = c("Short, Negative", "Short, Positive", "Chosen, Negative", "Chosen, Positive", "Long, Negative", "Long, Positive")
  )
  return(plot_data)
}

plotSpeciesWithTimeRange2 <- function(harvestedprojection1, harvestedprojection2,
                                          unharvestedprojection, chosentime1, chosentime2) {
  df1 <- process_sim(harvestedprojection1, unharvestedprojection, chosentime1, chosentime2)
  df2 <- process_sim(harvestedprojection2, unharvestedprojection, chosentime1, chosentime2)

  df1$sim <- "Sim 1"
  df2$sim <- "Sim 2"

  plot_df <- bind_rows(df1, df2)

  p <- ggplot(plot_df, aes(x = Species, y = percentage_diff, fill = fill_group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", linewidth = 0.5) +
    labs(x = "Species", y = "% Change") +
    scale_fill_manual(values = c(
      "Short, Negative"  = "#F2A488",
      "Short, Positive"  = "#2FA4E799",
      "Chosen, Negative" = "#E98C6B",
      "Chosen, Positive" = "#2FA4E7cc",
      "Long, Negative"   = "#E76F51",
      "Long, Positive"   = "#2FA4E7"
    )) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          legend.position = "none",
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          panel.spacing.y = unit(2, "lines")) +
    facet_wrap(~ sim,nrow = 2)

  return(p)
}
