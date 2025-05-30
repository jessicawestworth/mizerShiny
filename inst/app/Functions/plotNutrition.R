
plotNutrition <- function(sims, ref, step) {
  nut_cols <- setdiff(names(nut), "species")

  tot_nuts <- function(sim) {
    y <- getYield(sim)

    yield_df <- tibble(
      fmp   = colnames(y),
      Yield = colMeans((y[step, , drop = FALSE]))
    )

    nut_cols <- setdiff(names(nut), "species")

    yield_df %>%
      left_join(all_matches,  by = "fmp") %>%
      left_join(nut,           by = c(nut_match = "species")) %>%
      mutate(across(all_of(nut_cols), ~ .x * Yield) ) %>%
      summarise(across(all_of(nut_cols), ~ sum(.x, na.rm = TRUE)))%>%
      unlist(use.names = TRUE)
  }

  ref_tot  <- tot_nuts(ref)[nut_cols]

  sim_tots  <- lapply(sims, tot_nuts)

  rel_list <- lapply(seq_along(sim_tots), function(i) {
    tibble(
      Nutrient = nut_cols,
      Relative = sim_tots[[i]][nut_cols] / ref_tot,
      Sim      = paste0("Sim ", i)
    )
  })
  plot_dat <- bind_rows(rel_list)%>%
    filter(!is.na(Relative))

  dodge <- if (length(sims) == 2) position_dodge(width = 0.7) else "identity"

  plot_dat <- plot_dat%>%
                 mutate(Value = (Relative - 1) * 100)

  ggplot(plot_dat, aes(Nutrient, Value, fill = Sim)) +
    geom_col(position = dodge, width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(y = "Relative value (%)", x = NULL) +
    #coord_cartesian(ylim = c(0, max(plot_dat$Relative) * 1.1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}


app_path <- function(...) {
  p <- system.file("app", ..., package = "mizerShiny")
  if (p == "") p <- file.path("inst", "app", ...)
  p
}

library(readr)
# determine the true file location inside the package or dev tree
nutrition_file <- app_path(
  "Including", "Nutrition", "checkNutrition", "nutrition.csv"
)

if (file.exists(nutrition_file)) {
  nut <- read_csv(
    nutrition_file,
    locale        = locale(encoding = "ISO-8859-1"),
    show_col_types = FALSE
  ) %>%
    select(
      common_name,
      matches("\\(")        # keep only columns with parentheses
    ) %>%
    rename_with(
      ~ toupper(gsub("\\s*\\(.*\\)", "", .x)),
      everything()
    ) %>%
    rename(
      species = COMMON_NAME
    )
} else {
  stop("Nutrition CSV not found at: ", nutrition_file)
}

#this file links the species names in the model to the species names.
#this is only necessary here because the names are different for certain species and they all use common names,
#whereas the nutrition file uses the scientific names. - put this into
#load("Including/Nutrition/nutrition_match_northsea.RData")

match_dir  <- app_path("Including", "Nutrition", "nutritionMatch")
match_file <- list.files(match_dir, pattern = "\\.RData$", full.names = TRUE)

if (length(match_file) == 1L) {
  tmp <- new.env()
  obj <- load(match_file, envir = tmp)[1]   # first / only object in the file
  all_matches <- tmp[[obj]]                 # standard name used below
} else {
  stop("Need exactly one *.RData file in ", match_dir)
}

#this code is how i got the lists that translate between different names
#it is done some manually some automatically.

# library(dplyr)
# library(stringr)
# library(fuzzyjoin)

#fmp15 is the mizer model.

# fmp    <- FMP_15@species_params$species
# #fmp <- NS_params@species_params$species
# nutsp  <- nut$species
#
# pre_matches <- tibble(fmp = fmp) %>%
#   rowwise() %>%
#   mutate(
#     nut_match = {
#       # look for any nutsp that contains the fmp name
#       hits <- nutsp[str_detect(str_to_lower(nutsp),
#                                fixed(str_to_lower(fmp)))]
#       if (length(hits) > 0) hits[1] else NA_character_
#     }
#   ) %>%
#   ungroup()
#
# # Step 2: pull out the leftovers and do a fuzzy‐join on those
# leftovers <- pre_matches %>%
#   filter(is.na(nut_match)) %>%
#   select(fmp)
#
# fuzzy_matches <- leftovers %>%
#   # need a column named the same on both sides, so call it tmp
#   mutate(tmp = fmp) %>%
#   stringdist_inner_join(
#     tibble(nutsp = nutsp),
#     by           = c("tmp" = "nutsp"),
#     method       = "jw",
#     max_dist     = 0.25,    # you can tweak this threshold
#     ignore_case  = TRUE,
#     distance_col = "dist"
#   ) %>%
#   group_by(fmp) %>%
#   slice_min(dist, with_ties = FALSE) %>%
#   ungroup() %>%
#   select(fmp, nut_match = nutsp)
#
# # Step 3: combine the two
# all_matches <- bind_rows(
#   pre_matches    %>% filter(!is.na(nut_match)) %>% select(fmp, nut_match),
#   fuzzy_matches
# )
#
# all_matches
#
# #now add in any manually that this misses.
# all_matches <- rbind(all_matches,c("Pilchard","Sardines"))
# #all_matches <- rbind(all_matches, c("N.pout", "Norway pout"))
# save(all_matches, file="nutrition_match_celticsea.RData")



#read in the nutrion info (prior)

#using your own nutrition file should work if you put it in this location,
#and all it contains is a species column with each other column being nutrients.

#the information plotted here is the nutrient quantity relatively across species.
#this is multiplied by the yield of each species to get the total nutrient composition and quantity.
#so all that is necessary in your file is species name - and the amount of nutrient for your chosen nutrients
#in a given quantity of that species (the same quantity for each species)

#the nutrition data here is from multiple sources
#Robinson JPW, Garrett A, Esclapez JCP, Maire E, Parker RWR and Graham NAJ. Navigating sustainability and health trade-offs in global seafood systems (2022)
#seafish database
#ufish database
#fishbase database
#AnFooD2.0 FAO database







