guildplot <- function(harvestedprojection, unharvestedprojection,
                      year1, year2,
                      guildparams, celticsim,
                      mode = c("chosen", "triple")) {

  mode <- match.arg(mode)

  harvested    <- plotSpectra(harvestedprojection,
                              time_range = year1:year2,
                              return_data = TRUE)
  unharvested  <- plotSpectra(unharvestedprojection,
                              time_range = year1:year2,
                              return_data = TRUE)

  if (mode == "triple") {
    midpoint   <- ceiling((year1 + year2) / 2)

    mid_short  <- max(1, midpoint %/% 2 - 1) : (midpoint %/% 2 + 1)
    mid_long   <- (midpoint * 2 - 1) : (midpoint * 2 + 1)

    harvestedshort   <- plotSpectra(harvestedprojection,
                                    time_range = mid_short,
                                    return_data = TRUE)
    harvestedlong    <- plotSpectra(harvestedprojection,
                                    time_range = mid_long,
                                    return_data = TRUE)
    unharvestedshort <- plotSpectra(unharvestedprojection,
                                    time_range = mid_short,
                                    return_data = TRUE)
    unharvestedlong  <- plotSpectra(unharvestedprojection,
                                    time_range = mid_long,
                                    return_data = TRUE)
  }

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
      dplyr::group_modify(\(.x, .y){
        rules <- guildparams |>
          dplyr::filter(Species == unique(.x$Legend))
        if (nrow(rules) == 0) .x else assign_guild(.x, rules)
      }) |>
      dplyr::ungroup() |>
      tidyr::drop_na(Guild) |>
      dplyr::group_by(Guild) |>
      dplyr::summarise(value = mean(value), .groups = "drop")
  }

  guilds    <- process_guilds(harvested)    |> dplyr::mutate(time = "chosen")
  unguilds  <- process_guilds(unharvested)  |> dplyr::mutate(time = "chosen")

  if (mode == "triple") {
    guildsshort <- process_guilds(harvestedshort)   |> dplyr::mutate(time = "short")
    guildslong  <- process_guilds(harvestedlong)    |> dplyr::mutate(time = "long")
    unguildsshort <- process_guilds(unharvestedshort) |> dplyr::mutate(time = "short")
    unguildslong  <- process_guilds(unharvestedlong)  |> dplyr::mutate(time = "long")

    harv_all   <- dplyr::bind_rows(guilds, guildsshort, guildslong)
    unharv_all <- dplyr::bind_rows(unguilds, unguildsshort, unguildslong)
  } else {                       # mode == "chosen"
    harv_all   <- guilds
    unharv_all <- unguilds
  }

  joinedguilds <- harv_all |>
    dplyr::full_join(unharv_all, by = c("Guild","time")) |>
    dplyr::mutate(percentage_diff = (value.x - value.y) / value.y * 100) |>
    dplyr::select(Guild, time, percentage_diff)

  joinedguilds$time <- factor(
    joinedguilds$time,
    levels = if (mode == "chosen") "chosen" else c("short","chosen","long")
  )
  joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0,
                                         joinedguilds$time)

  joinedguilds$Class <- factor(
    joinedguilds$fill_group,
    levels = c("FALSE.short", "TRUE.short", "FALSE.chosen", "TRUE.chosen", "FALSE.long", "TRUE.long"),
    labels = c("Short, Negative", "Short, Positive", "Chosen, Negative", "Chosen, Positive", "Long, Negative", "Long, Positive")
  )

  joinedguilds$Percentage <- joinedguilds$percentage_diff

  ## ---- plot -----------------------------------------------------------------
  ggplot(joinedguilds, aes(Guild, Percentage, fill = Class)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, colour = "grey", linetype = "dashed", linewidth = 0.5) +
    scale_fill_manual(
      values = c(
        "Short, Negative"  = "#E76F51",
        "Short, Positive"  = "#2FA4E7",
        "Chosen, Negative" = "#E98C6B",
        "Chosen, Positive" = "#2FA4E7cc",
        "Long, Negative"   = "#F2A488",
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
    )

}

##HOW TO LOAD PUT IN THE GUILDS STUFF
#This loads and formats in the data for the guilds
#
# guildinfo <- read.table("Guilds information/guild_cleaned.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# fish_names <- read.table("Guilds information/fishinfo.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#
# guildparams <- celticsim@species_params%>%
#   select(species, a, b)%>%
#   rename(Common_Name=species)%>%
#   #this code has correctly formatted the species params
#   inner_join(fish_names, by=c("Common_Name"))%>%
#   #we have now joined species params to a table containing the scientific names
#   rename(Species=Scientific_Name)%>%
#   inner_join(
#     guildinfo%>%
#       filter(Species %in% fish_names$Scientific_Name),
#     by="Species")%>%
#   #we have now joined the rows with the same scientific names - so
#   #we have joined the a and b values to the given species
#   #this is converting from length to weight
#   mutate(maxw=a*Max.cm^b,
#          minw=a*Min.cm^b)%>%
#   select(Common_Name, maxw, minw, Feeding.guild)%>%
#   rename(Species=Common_Name)

#So what the code does above is take the table of the guild information,
#from the pilot assessment by Murray Thompson, then you give it a table
#containing the species common + scientific names, and this is all then
#joined together so you have the a and b values next to given species,
#so therefore we are able to convert from the length measurements to weight,
#which can then be used in mizer to filter into the correct guilds.


