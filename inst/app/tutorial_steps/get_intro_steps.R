get_intro_steps <- function(input) {
  # Ensure required inputs are available
  req(input$bigtabpanel, input$mortnsp_tab)
  
  intro_steps <- list()  # Initialize an empty list
  
  if (input$bigtabpanel == "Single Species") {
    if (input$mortnsp_tab == "Biomass") {
      source("tutorial_steps/single_species_biomass.R", local = TRUE)
    } else if (input$mortnsp_tab == "Mortality") {
      source("tutorial_steps/single_species_mortality.R", local = TRUE)
    }
  } else if (input$bigtabpanel == "Breakpoint") {
    if (input$breakpoint_tabpanel == "Biomass") {
      source("tutorial_steps/breakpoint_biomass.R", local = TRUE)
    } else if (input$breakpoint_tabpanel == "Mortality") {
      source("tutorial_steps/breakpoint_mortality.R", local = TRUE)
    }
  } else if (input$bigtabpanel == "Fishery Strategy") {
    source("tutorial_steps/fishery_strategy.R", local = TRUE)
  }
  
  # The sourced file should create the variable 'intro_steps'
  return(intro_steps)
}
