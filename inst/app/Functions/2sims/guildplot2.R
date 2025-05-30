guildplot_both <- function(harvestedprojection1, harvestedprojection2, unharvestedprojection, 
                           year1, year2, guildparams, celticsim) {
  
  ## Helper function to process a mizerSim object into guild-level summaries 
  process_guild <- function(spectrum_data) {
    # Assign guilds according to the provided rules
    assign_guild <- function(data, rules) {
      data <- data %>% mutate(Guild = NA_character_)
      for (i in seq_len(nrow(rules))) {
        data <- data %>% 
          mutate(
            Guild = ifelse(w < 0.05, "Plank",
                           ifelse(is.na(Guild) & w >= rules$minw[i] & w < rules$maxw[i],
                                  rules$Feeding.guild[i],
                                  Guild))
          )
      }
      return(data)
    }
    
    processed <- spectrum_data %>% 
      group_by(Species) %>% 
      group_modify(~ {
        species_data <- .x
        species_name <- unique(species_data$Legend)
        species_rules <- guildparams %>% filter(Species == species_name)
        if(nrow(species_rules) == 0) return(species_data)
        assign_guild(species_data, species_rules)
      }) %>% 
      ungroup() %>%
      drop_na(Guild) %>%
      group_by(Guild) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    return(processed)
  }
  
  ## Process spectra for harvested and unharvested simulations at three time ranges
  
  # For sim1
  hs_short1   <- mizer::plotSpectra(harvestedprojection1, time_range = max(1, round(year1 * 0.5)):max(1, round(year2 * 0.5)), return_data = TRUE)
  hs_chosen1  <- mizer::plotSpectra(harvestedprojection1, time_range = year1:year2, return_data = TRUE)
  hs_long1    <- mizer::plotSpectra(harvestedprojection1, time_range = (year1 * 2):(year2 * 2), return_data = TRUE)
  
  us_short    <- mizer::plotSpectra(unharvestedprojection, time_range = max(1, round(year1 * 0.5)):max(1, round(year2 * 0.5)), return_data = TRUE)
  us_chosen   <- mizer::plotSpectra(unharvestedprojection, time_range = year1:year2, return_data = TRUE)
  us_long     <- mizer::plotSpectra(unharvestedprojection, time_range = (year1 * 2):(year2 * 2), return_data = TRUE)
  
  # Process guild levels for sim1
  guilds_short1   <- process_guild(hs_short1)
  guilds_chosen1  <- process_guild(hs_chosen1)
  guilds_long1    <- process_guild(hs_long1)
  
  # Process guild levels for unharvested simulation
  unguilds_short  <- process_guild(us_short)
  unguilds_chosen <- process_guild(us_chosen)
  unguilds_long   <- process_guild(us_long)
  
  # Add time labels for sim1 data
  guilds_short1$time  <- "short"
  guilds_chosen1$time <- "chosen"
  guilds_long1$time   <- "long"
  unguilds_short$time  <- "short"
  unguilds_chosen$time <- "chosen"
  unguilds_long$time   <- "long"
  
  sim1_data <- bind_rows(guilds_short1, guilds_chosen1, guilds_long1) %>% 
    group_by(Guild, time) %>% 
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  unguilds <- bind_rows(unguilds_short, unguilds_chosen, unguilds_long) %>% 
    group_by(Guild, time) %>% 
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  sim1_final <- full_join(sim1_data, unguilds, by = c("Guild", "time"), relationship = "many-to-many") %>%
    mutate(percentage_diff = (value.x - value.y)/value.y) %>%
    select(Guild, time, percentage_diff) %>%
    mutate(sim = "Sim 1")
  
  # For sim2
  hs_short2   <- mizer::plotSpectra(harvestedprojection2, time_range = max(1, round(year1 * 0.5)):max(1, round(year2 * 0.5)), return_data = TRUE)
  hs_chosen2  <- mizer::plotSpectra(harvestedprojection2, time_range = year1:year2, return_data = TRUE)
  hs_long2    <- mizer::plotSpectra(harvestedprojection2, time_range = (year1 * 2):(year2 * 2), return_data = TRUE)
  
  guilds_short2   <- process_guild(hs_short2)
  guilds_chosen2  <- process_guild(hs_chosen2)
  guilds_long2    <- process_guild(hs_long2)
  
  guilds_short2$time  <- "short"
  guilds_chosen2$time <- "chosen"
  guilds_long2$time   <- "long"
  
  sim2_data <- bind_rows(guilds_short2, guilds_chosen2, guilds_long2) %>% 
    group_by(Guild, time) %>% 
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  sim2_final <- full_join(sim2_data, unguilds, by = c("Guild", "time"), relationship = "many-to-many") %>%
    mutate(percentage_diff = (value.x - value.y)/value.y) %>%
    select(Guild, time, percentage_diff) %>%
    mutate(sim = "Sim 2")
  
  # Combine the two sets of data
  final_df <- bind_rows(sim1_final, sim2_final)
  final_df$time <- factor(final_df$time, levels = c("short", "chosen", "long"))
  final_df$fill_group <- interaction(final_df$percentage_diff >= 0, final_df$time)
  final_df$fill_group <- factor(
    final_df$fill_group,
    levels = c("FALSE.short", "FALSE.chosen", "FALSE.long", "TRUE.short", "TRUE.chosen", "TRUE.long"),
    labels = c("Short, Negative", "Chosen, Negative", "Long, Negative",
               "Short, Positive", "Chosen, Positive", "Long, Positive")
  )
  
  p <- ggplot(final_df, aes(x = Guild, y = percentage_diff * 100, fill = fill_group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 0.5) +
    labs(title = "", x = "Guild", y = "Percentage Change") +
    scale_fill_manual(values = c(
      "Short, Negative"  = "#E76F51",
      "Chosen, Negative" = "#E98C6B",
      "Long, Negative"   = "#F2A488",
      "Short, Positive"  = "#2FA4E7",
      "Chosen, Positive" = "#2FA4E7cc",
      "Long, Positive"   = "#2FA4E799"
    )) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          legend.position = "none",
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16)) +
    facet_wrap(~ sim)
  
  return(p)
}
