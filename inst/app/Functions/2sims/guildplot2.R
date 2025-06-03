guildplot_both <- function(harvestedprojection1, harvestedprojection2,
                           unharvestedprojection,
                           year1, year2,
                           guildparams, celticsim,
                           mode = c("chosen", "triple")) {

  mode <- match.arg(mode)

  process_guilds <- function(mizerprojection) {

    assign_guild <- function(dat, rules) {
      dat <- dat |> dplyr::mutate(Guild = NA_character_)
      for (i in seq_len(nrow(rules))) {
        dat <- dat |>
          dplyr::mutate(
            Guild = dplyr::case_when(
              w < 0.05                                      ~ "Plank",
              is.na(Guild) &
                w >= rules$minw[i] & w < rules$maxw[i]      ~ rules$Feeding.guild[i],
              TRUE                                          ~ Guild
            )
          )
      }
      dat
    }

    mizerprojection |>
      dplyr::group_by(Species) |>
      dplyr::group_modify(\(.x, .y) {
        rules <- guildparams |>
          dplyr::filter(Species == unique(.x$Legend))
        if (nrow(rules) == 0) .x else assign_guild(.x, rules)
      }) |>
      dplyr::ungroup() |>
      tidyr::drop_na(Guild) |>
      dplyr::group_by(Guild) |>
      dplyr::summarise(value = mean(value), .groups = "drop")
  }

  if (mode == "chosen") {
    harvestedchosen1 <- plotSpectra(harvestedprojection1,
                                    time_range = year1:year2,
                                    return_data = TRUE)
    harvestedchosen2 <- plotSpectra(harvestedprojection2,
                                    time_range = year1:year2,
                                    return_data = TRUE)
    unharvestedchosen <- plotSpectra(unharvestedprojection,
                                     time_range = year1:year2,
                                     return_data = TRUE)

    guilds_chosen1  <- process_guilds(harvestedchosen1)  |> dplyr::mutate(time = "chosen")
    guilds_chosen2  <- process_guilds(harvestedchosen2)  |> dplyr::mutate(time = "chosen")
    unguilds_chosen <- process_guilds(unharvestedchosen) |> dplyr::mutate(time = "chosen")

    harv1_all <- guilds_chosen1
    harv2_all <- guilds_chosen2
    unharv_all <- unguilds_chosen

    sim1_final <- harv1_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 1")

    sim2_final <- harv2_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 2")

    joinedguilds <- dplyr::bind_rows(sim1_final, sim2_final)

  } else {  # mode == "triple"

    hs_short1   <- plotSpectra(harvestedprojection1,
                               time_range = max(1, round(year1 * 0.5)) : max(1, round(year2 * 0.5)),
                               return_data = TRUE)
    hs_chosen1  <- plotSpectra(harvestedprojection1,
                               time_range = year1:year2,
                               return_data = TRUE)
    hs_long1    <- plotSpectra(harvestedprojection1,
                               time_range = (year1 * 2):(year2 * 2),
                               return_data = TRUE)

    hs_short2   <- plotSpectra(harvestedprojection2,
                               time_range = max(1, round(year1 * 0.5)) : max(1, round(year2 * 0.5)),
                               return_data = TRUE)
    hs_chosen2  <- plotSpectra(harvestedprojection2,
                               time_range = year1:year2,
                               return_data = TRUE)
    hs_long2    <- plotSpectra(harvestedprojection2,
                               time_range = (year1 * 2):(year2 * 2),
                               return_data = TRUE)

    us_short    <- plotSpectra(unharvestedprojection,
                               time_range = max(1, round(year1 * 0.5)) : max(1, round(year2 * 0.5)),
                               return_data = TRUE)
    us_chosen   <- plotSpectra(unharvestedprojection,
                               time_range = year1:year2,
                               return_data = TRUE)
    us_long     <- plotSpectra(unharvestedprojection,
                               time_range = (year1 * 2):(year2 * 2),
                               return_data = TRUE)

    guilds_short1   <- process_guilds(hs_short1)   |> dplyr::mutate(time = "short")
    guilds_chosen1  <- process_guilds(hs_chosen1)  |> dplyr::mutate(time = "chosen")
    guilds_long1    <- process_guilds(hs_long1)    |> dplyr::mutate(time = "long")

    guilds_short2   <- process_guilds(hs_short2)   |> dplyr::mutate(time = "short")
    guilds_chosen2  <- process_guilds(hs_chosen2)  |> dplyr::mutate(time = "chosen")
    guilds_long2    <- process_guilds(hs_long2)    |> dplyr::mutate(time = "long")

    unguilds_short  <- process_guilds(us_short)    |> dplyr::mutate(time = "short")
    unguilds_chosen <- process_guilds(us_chosen)   |> dplyr::mutate(time = "chosen")
    unguilds_long   <- process_guilds(us_long)     |> dplyr::mutate(time = "long")

    harv1_all <- dplyr::bind_rows(guilds_short1, guilds_chosen1, guilds_long1) |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop")

    harv2_all <- dplyr::bind_rows(guilds_short2, guilds_chosen2, guilds_long2) |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop")

    unharv_all <- dplyr::bind_rows(unguilds_short, unguilds_chosen, unguilds_long) |>
      dplyr::group_by(Guild, time) |>
      dplyr::summarise(value = sum(value), .groups = "drop")

    sim1_final <- harv1_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 1")

    sim2_final <- harv2_all |>
      dplyr::full_join(unharv_all, by = c("Guild", "time")) |>
      dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
      dplyr::select(Guild, time, percentage_diff) |>
      dplyr::mutate(sim = "Sim 2")

    joinedguilds <- dplyr::bind_rows(sim1_final, sim2_final)

  }

  joinedguilds$time <- factor(
    joinedguilds$time,
    levels = c("short", "chosen", "long")
  )
  joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0,
                                         joinedguilds$time)

  joinedguilds$Class <- factor(
    joinedguilds$fill_group,
    levels = c("FALSE.short", "FALSE.chosen", "FALSE.long",
               "TRUE.short", "TRUE.chosen", "TRUE.long"),
    labels = c("Short, Negative", "Chosen, Negative", "Long, Negative",
               "Short, Positive", "Chosen, Positive", "Long, Positive")
  )

  joinedguilds$Percentage <- joinedguilds$percentage_diff

  ggplot(joinedguilds, aes(Guild, Percentage, fill = Class)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, colour = "grey", linetype = "dashed", size = 0.5) +
    scale_fill_manual(
      values = c(
        "Short, Negative"  = "#E76F51",
        "Chosen, Negative" = "#E98C6B",
        "Long, Negative"   = "#F2A488",
        "Short, Positive"  = "#2FA4E7",
        "Chosen, Positive" = "#2FA4E7cc",
        "Long, Positive"   = "#2FA4E799"
      ),
      drop = FALSE
    ) +
    labs(x = "Guild", y = "Percentage Change") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title = element_text(size = 16)
    ) +
    facet_wrap(~ sim,nrow = 2)

}
