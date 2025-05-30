#' Plot MizerSim Relative Biomass per Species Across Varying Timescales
#'
#' This function takes two mizerSim objects and calculates the relative %
#' change in each given species in the chosen year, short term (1/2 of the
#' chosen year) and the long term (2x the chosen year)
#'
#' @param harvested A mizerSim object
#' @param unharvested A mizerSim object - to compare to.
#' @param chosentime The year to plot
#'
#' @return A ggplot object that plots 3 bars per species - in the short,
#' chosen and long time - it plots the relative biomass of each species in
#' comparison to the unharvested.
#'
#'
#' @examples
#' harvested <- getBiomass(NS_sim)
#' unharvested <- getBiomass(NS_sim)
#' percentdiff(harvested, unharvested)
#'
#' @export
plotSpeciesWithTimeRange <- function(harvestedprojection, unharvestedprojection, chosentime1, chosentime2,
                                     mode = c("triple", "chosen")) {

  mode <- match.arg(mode)

  #get the biomass of the species
  unharvestedbio <- getBiomass(unharvestedprojection) %>%
    .[chosentime1:chosentime2, ] %>%
    melt() %>%
    group_by(sp) %>%
    summarize(value = mean(value, na.rm = TRUE))

  harvestedbio <- getBiomass(harvestedprojection) %>%
    .[chosentime1:chosentime2, ] %>%
    melt() %>%
    group_by(sp) %>%
    summarize(value = mean(value, na.rm = TRUE))

  #calculate percentage change in species in the chosen year
  percentage_diff <-  harvestedbio %>%
    left_join(unharvestedbio, by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% ("Resource"))%>%
    mutate(class = "chosen")

  calculate_biomass_triples <- function(unharvestedprojection, harvestedprojection, year1, year2) {# Calculate mid-year and enforce a 3-year time window
    midyear <- (year1 + year2) / 2

    # Define 3-year time window for 0.5x and 2x resolution
    low_start <- ceiling(midyear * 0.5) - 1
    low_end   <- ceiling(midyear * 0.5) + 1

    high_start <- midyear * 2 - 1
    high_end   <- midyear * 2 + 1

    # Unharvested biomass
    unharvestedbiotriple <- getBiomass(unharvestedprojection)

    lowunbiotrip <- unharvestedbiotriple[low_start:low_end, ] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))

    highunbiotrip <- unharvestedbiotriple[high_start:high_end, ] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))

    # Harvested biomass
    harvestedbiotriple <- getBiomass(harvestedprojection)

    lowbiotrip <- harvestedbiotriple[low_start:low_end, ] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))

    highbiotrip <- harvestedbiotriple[high_start:high_end, ] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))

    # Return as list
    list(
      lowunbiotrip,
      highunbiotrip,
      lowbiotrip,
      highbiotrip
    )
  }


  if (mode == "triple") {
  #calculate percentage change in other years
  biorange <- calculate_biomass_triples(unharvestedprojection, harvestedprojection, chosentime1, chosentime2)

  percentage_difflow <-  biorange[[3]] %>%
    left_join(biorange[[1]], by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% ("Resource"))%>%
    mutate(class = "short")


  percentage_diffhigh <-  biorange[[4]] %>%
    left_join(biorange[[2]], by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% ("Resource"))%>%
    mutate(class = "long")

  percentage_diff <- rbind(percentage_difflow, percentage_diff, percentage_diffhigh)
}
  #now plot them together - the first lines sort out the colors of the bars
  percentage_diff$class <- factor(percentage_diff$class, levels = c("short", "chosen", "long"))
  percentage_diff$fill_group <- interaction(percentage_diff$percentage_diff >= 0, percentage_diff$class)

  percentage_diff$fill_group <- factor(
    percentage_diff$fill_group,
    levels = c("FALSE.short", "FALSE.chosen", "FALSE.long",
               "TRUE.short", "TRUE.chosen", "TRUE.long"),
    labels = c("Short, Negative", "Chosen, Negative", "Long, Negative",
               "Short, Positive", "Chosen, Positive", "Long, Positive")
  )

  percentage_diff$Percentage <- percentage_diff$percentage_diff
  percentage_diff$Class <- percentage_diff$fill_group
  ggplot(percentage_diff, aes(x = Species, y = Percentage, fill = Class)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 0.5)+
    labs(x = "Species", y = "% Change") +
    scale_fill_manual(values = c(
      "Short, Negative" = "#E76F51",
      "Chosen, Negative" = "#E98C6B",
      "Long, Negative" = "#F2A488",
      "Short, Positive" = "#2FA4E7",
      "Chosen, Positive" = "#2FA4E7cc",
      "Long, Positive" = "#2FA4E799"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 13, angle = 45, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )
}
