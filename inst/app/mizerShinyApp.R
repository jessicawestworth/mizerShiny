library(shiny)
library(mizer)
library(ggplot2)
library(bslib)
library(plotly)
library(ggplot2)
library(gridlayout)
library(thematic)
library(tidyverse)
library(forcats)
library(shinyBS)
library(rintrojs)
library(patchwork)
library(here)
library(sortable)
library(shinyjs)
library(shinyWidgets)
library(dplyr)

#Functions to help load in files
app_path <- function(...) {
  p <- system.file("app", ..., package = "mizerShiny")
  if (p == "") p <- file.path("inst", "app", ...); p
}
app_exists <- function(...) file.exists(app_path(...))

sim_path <- function(name) app_path(
  "Including", "mizerSim", name,
  paste0(tolower(name), "projection.RData")
)

#Functions to help with running new mizer models
sim_species <- function(sim) dimnames(sim@n)$sp
new_projection <- function()
  project(celticsim, t_max = 202, effort = celticsim@initial_effort)

## main loader - loads in mizer objects from folders
load_or_project <- function(subdir, varname) {

  ## 0. already supplied by mizerShiny() ?  -------------------------------
  if (exists(varname, envir = .GlobalEnv) &&
      inherits(get(varname, .GlobalEnv), "MizerSim")) {

    sim <- get(varname, .GlobalEnv)

    if (!setequal(sim_species(sim), celticsim@species_params$species)) {
      sim <- new_projection()
      message("Species mismatch – regenerated ", varname)
    }

    assign(varname, sim, envir = .GlobalEnv)
    return(invisible())
  }

  ##try to load it from disk ------------------------------------------
  f <- sim_path(subdir)
  if (file.exists(f)) {
    tmp <- new.env()
    obj <- load(f, envir = tmp)[1]
    sim <- tmp[[obj]]

    if (!setequal(sim_species(sim), celticsim@species_params$species)) {
      sim <- new_projection()
      message("Species mismatch in ", basename(f), " – regenerated")
    }

  } else {                                     #Fall back to projection
    sim <- new_projection()
    message("No saved ", subdir,
            " projection – generated a fresh one")
  }

  assign(varname, sim, envir = .GlobalEnv)
  invisible()
}

load_or_project("Unharvested", "unharvestedprojection")
load_or_project("Unfished",    "unfishedprojection")

#Find years for the Time Range
sp_max_year   <- floor((dim(unharvestedprojection@n)[1] - 2) / 2)
fish_max_year <- floor((dim(unfishedprojection  @n)[1] - 2) / 2)

# guild & nutrition data ----------------------------------------
guild_file <- app_path("Including", "guilds_information", "checkGuilds",
                       "guildparams_preprocessed.Rdata")
have_guild_file <- file.exists(guild_file)
if (have_guild_file) load(guild_file, envir = .GlobalEnv)

nut_file <- app_path("Including", "Nutrition", "checkNutrition", "nutrition.csv")
have_nutrition_file <- file.exists(nut_file)
#Loading in text for legends and the code to generate them
source(app_path("www", "legendsTXT.R"), local = TRUE)
source(app_path("Functions", "legendUI.R"), local = TRUE)

server <- function(input, output, session) {

  #loading the introduction/guide
  source("tutorial_steps/get_intro_steps.R")
  observeEvent(input$start_tutorial, {
    steps <- get_intro_steps(input)

    if (length(steps) > 0) {
      introjs(session, options = list(steps = steps))
    }
  })

  #changing the species options to dynamically change depending on the model
  species_list <- reactive({
    setdiff(unique(celticsim@species_params$species),
            ("Resource"))
  })
  species_input_ids <- c(
    "species_name_select", "name_select",
    "fish_name_select",
    "diet_species_select",
    "diet_species_select_mort"
  )
  observe({
    lapply(species_input_ids, function(id) {
      updateSelectInput(session, id, choices = species_list())
    })
  })

  #changing the fishery options to dynamically change depending on the model
  output$fishery_sliders_ui <- renderUI({
    effort <- celticsim@initial_effort
    gears <- unique(celticsim@gear_params$gear)
    slider_list <- lapply(gears, function(gear) {
      sliderInput(
        inputId = paste0("effort_", gear),  # e.g., "effort_total"
        label = paste("Effort for", gear),
        min = 0,
        #if its 0, it needs a different way.
        max = if(celticsim@initial_effort[gear]==0){
          2
        }else(celticsim@initial_effort[gear]*2),
        value = celticsim@initial_effort[gear],
        step = 0.05,
        width = "100%"
      )
    })
    div(id = "fishery_sliders", slider_list)
  })
  # for both sections
  output$fishery_sliders_ui2 <- renderUI({
    effort <- celticsim@initial_effort
    gears <- unique(celticsim@gear_params$gear)
    slider_list <- lapply(gears, function(gear) {
      sliderInput(
        inputId = paste0("effort2_", gear),  # e.g., "effort_total"
        label = paste("Effort for", gear),
        min = 0,
        max = if(celticsim@initial_effort[gear]==0){
          2
        }else(celticsim@initial_effort[gear]*2),
        value = celticsim@initial_effort[gear],
        step = 0.05,
        width = "100%"
      )
    })
    div(id = "fishery_sliders", slider_list)
  })

  #This section contains all the functions to be used
  source("Functions/plotSpectraRelative.R")
  source("Functions/percentdiff.R")
  source("Functions/plotSpeciesWithTimeRange.R")
  source("Functions/guildplot.R")
  source("Functions/comparedietmatrix.R")
  source("Functions/create_species_level_plot.R")
  source("Functions/yieldplot.R")
  source("Functions/plotDietwComparison.R")
  source("Functions/plotNutrition.R")
  source("Functions/yearControls.R")

  source("Functions/2sims/plotSpeciesWithTimeRange2.R")
  source("Functions/2sims/create_species_level_plot2.R")
  source("Functions/2sims/plotSpectraRelative2.R")
  source("Functions/2sims/guildplot2.R")
  source("Functions/2sims/plotSpectra2.R")

  #Having a custom order on the X axis
  contexts <- c("bio", "mort", "fish")
  custom_species_order <- reactiveVal(NULL)
  species_order_choice <- reactiveVal("Unordered")

  #add the ui to load when custom is pressed
  lapply(contexts, function(ctx) {
    observeEvent(input[[paste0("customOrderInfo_", ctx)]], {
      showModal(modalDialog(
        title = "Customise species order",
        rank_list(
          input_id = paste0("custom_order_rank_", ctx),
          text     = "Drag the species into the order you want:",
          labels   = species_list()
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(paste0("save_custom_order_", ctx), "Save order")
        ),
        easyClose = TRUE
      ))
    })
  })
  #save this order and change the menu so Chosen is first
  lapply(contexts, function(ctx) {
    observeEvent(input[[paste0("save_custom_order_", ctx)]], {
      custom_species_order(input[[paste0("custom_order_rank_", ctx)]])
      for (x in contexts)
        updateSelectInput(session, paste0("species_order_", x), selected = "Chosen")
      removeModal()
    })
  })
  #now the custom order thats been saved is made to be the species order
  lapply(contexts, function(ctx) {
    observeEvent(input[[paste0("species_order_", ctx)]], {
      species_order_choice(input[[paste0("species_order_", ctx)]])
    }, ignoreInit = TRUE)
  })
  #sets the order chosen by the user
  ordered_species <- reactive({
    choice <- species_order_choice()

    if (choice == "Unordered") {
      as.data.frame(celticsim@species_params$species) %>%
        setNames("sp") %>% filter(sp != "Resource") %>% pull(sp) %>% sample()

    } else if (choice == "Guild") {
      if (exists("guildparams", inherits = FALSE)) {
        guild_order <- guildparams |>
          ## keep only the size-class with the largest maxw for each species
          group_by(Species, Feeding.guild) |>
          slice_max(maxw, n = 1, with_ties = FALSE) |>
          ungroup() |>
          ## order by the guilds' first appearance, then pull the Species names
          arrange(factor(Feeding.guild,
                         levels = unique(Feeding.guild))) |>
          pull(Species) |>
          unique()

        ## return just the species that are actually in the current model
        intersect(guild_order, celticsim@species_params$species)

      } else { #if theres no guild infomration, this is the order if they choose guild
        as.data.frame(celticsim@species_params$species) |>
          setNames("sp") |>
          filter(sp != "Resource") |>
          pull(sp)
}
    } else if (choice == "Size") {
      celticsim@species_params %>%
        filter(species != "Resource") %>%
        arrange(w_mat) %>% pull(species)

    } else {
      ord <- custom_species_order()
      if (length(ord)) ord else
        as.data.frame(celticsim@species_params$species) %>%
        setNames("sp") %>% filter(sp != "Resource") %>% pull(sp)
    }
  })

  #will generate the +1/-1 and reset time buttons, function loaded from another file
  setupYearControls(
    input, session,
    sliderId = "year",
    runBtnId = "goButton1",
    boxId    = "yearAdjustButtons_bio",
    resetId  = "resetTimeYear_bio",
    minusId  = "decYear_bio",
    plusId   = "incYear_bio",
    rvName   = "rvBio"
  )


# Single species - Biomass ------------------------------------------------

  bioSimData <- eventReactive(input$goButton1, {
    speciessim <- celticsim

    time1 <- max(input$year - 1, 1)
    time2 <- input$year + 1

    pb <- shiny::Progress$new(); on.exit(pb$close())
    pb$set(message = "Running simulation …", value = 0)
    pb$inc(1/3, "Adjusting biomass …")

    speciessim@initial_n[input$species_name_select, ] <-
      speciessim@initial_n[input$species_name_select, ] * input$species

    pb$inc(1/3, "Projecting …")
    harvested <- project(
      speciessim,
      effort = unharvestedprojection@params@initial_effort,
      t_max  = time2 * 2
    )

    pb$inc(1/3, "Done")
    list(harvested = harvested,
         unharvested = unharvestedprojection)
  })

  #so if something breaks an error wont pop up, it will load the previous
  lastBioSpeciesPlot <- reactiveVal(NULL)
  lastBioSizePlot    <- reactiveVal(NULL)
  lastBioGuildPlot   <- reactiveVal(NULL)
  lastDietPlot     <- reactiveVal(NULL)

  #start of the output plots
  #they all generally follow the same order, so I have just commented this one.
  output$speciesPlot <- renderPlotly({
    #make sure we have model data and set the years
    req(bioSimData())
    t1 <- max(input$year - 1, 1)
    t2 <- input$year + 1
    #does user want 3 times plotted or one
    modeChoice <- if (isTRUE(input$triplotToggle)) "chosen" else "triple"

    #plot using the fucntion, but if its an error, load the previous plot
    p <- tryCatch({
      ggplotly(
        plotSpeciesWithTimeRange(
          bioSimData()$harvested,
          bioSimData()$unharvested,
          t1, t2,
          mode = modeChoice
        ) +
          scale_x_discrete(limits = ordered_species())
      )
    }, error = function(e) lastBioSpeciesPlot())

    lastBioSpeciesPlot(p)
    p
  })

  output$sizePlot <- renderPlotly({
    req(bioSimData())
    t1 <- max(input$year - 1, 1);  t2 <- input$year + 1

    p <- tryCatch({
      g <- plotSpectraRelative(
        bioSimData()$harvested,
        bioSimData()$unharvested,
        t1, t2
      )
      if (!isTRUE(input$logToggle))
        g <- g + scale_x_continuous()

      ggplotly(g)
    }, error = function(e) lastBioSizePlot())

    lastBioSizePlot(p); p
  })

  output$guildPlot <- renderPlotly({
    req(bioSimData())
    t1 <- max(input$year - 1, 1); t2 <- input$year + 1

    modeGuild <- if (isTRUE(input$triguildToggle)) "chosen" else "triple"

    p <- tryCatch({
      ggplotly(
        guildplot(
          bioSimData()$harvested, bioSimData()$unharvested,
          t1, t2,
          guildparams, celticsim,
          mode = modeGuild
        )
      )
    }, error = function(e) lastBioGuildPlot())

    lastBioGuildPlot(p); p
  })


  output$dietplot <- renderPlotly({
    req(bioSimData())
    win <- list(start = max(input$year - 1, 1), end = input$year + 1)
    sims <- list(bioSimData()$harvested, bioSimData()$unharvested)

    p <- tryCatch({
    harvest_sub <- lapply(sims, function(p) {
      p@n       <- p@n      [win$start:win$end, , , drop = FALSE]
      p@n_pp    <- p@n_pp   [win$start:win$end, ,      drop = FALSE]
      p@n_other <- p@n_other[win$start:win$end, ,      drop = FALSE]
      p
    })


        plotDietCompare(
          harvest_sub,
          species   = input$diet_species_select,
          sim_names = c("Your Sim", "Base Sim")
        )
      },
      error = function(e) {
        # on error, return the last successful diet plot
        lastDietPlot()
      }
    )
    # store and return
    lastDietPlot(p)
    p
  })


# Single Species - Mortality ----------------------------------------------

  ##THIS CODE IS NEAR IDENTICAL TO THE BIOMASS SECTION
  #Refer to comments above

  setupYearControls(
    input, session,
    sliderId = "mortyear",
    runBtnId = "goButton3",
    boxId    = "yearAdjustButtons",
    resetId  = "resetTimeYear",
    minusId  = "decYear",
    plusId   = "incYear",
    rvName   = "rvMort"
  )

  #Single Species - Mortality code.
  mortSimData <- eventReactive(input$goButton3, {
    speciessim <- celticsim
    time1 <- max(input$mortyear - 1, 1)
    time2 <- input$mortyear + 1

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running simulation...", value = 0)
    total_steps <- 3

    progress$inc(1/total_steps, "Updating mortality …")
    extmort   <- getExtMort(speciessim)
    totalmort <- getMort(speciessim)
    extmort[input$name_select, ] <- extmort[input$name_select, ] +
      (input$mortspecies * totalmort[input$name_select, ])
    ext_mort(speciessim) <- extmort

    progress$inc(1/total_steps, "Running simulation …")
    harvestedprojection <- project(
      speciessim,
      effort = unharvestedprojection@params@initial_effort,
      t_max  = time2 * 2
    )

    progress$inc(1/total_steps, "Done")

    list(
      harvested    = harvestedprojection,
      unharvested  = unharvestedprojection
    )
  })

  lastMortPlot <- reactiveVal(NULL)
  lastMortSizePlot  <- reactiveVal(NULL)
  lastMortGuildPlot <- reactiveVal(NULL)
  lastMortDietPlot     <- reactiveVal(NULL)

  output$mortspeciesPlot <- renderPlotly({
    req(mortSimData())
    t1 <- max(input$mortyear - 1, 1)
    t2 <- input$mortyear + 1

    # pick mode from our new toggle
    modeMort <- if (isTRUE(input$triplotToggleMort)) "chosen" else "triple"

    p <- tryCatch({
      ggplotly(
        plotSpeciesWithTimeRange(
          mortSimData()$harvested,
          mortSimData()$unharvested,
          t1, t2,
          mode = modeMort
        ) +
          scale_x_discrete(limits = ordered_species())
      )
    }, error = function(e) lastMortPlot())

    lastMortPlot(p)
    p
  })

  output$mortsizePlot <- renderPlotly({
    req(mortSimData())

    t1 <- max(input$mortyear - 1, 1)
    t2 <- input$mortyear + 1

    p <- tryCatch({
      g <- plotSpectraRelative(
        mortSimData()$harvested,
        mortSimData()$unharvested,
        t1, t2
      )
      if (!isTRUE(input$logToggle2))
        g <- g + scale_x_continuous()

      ggplotly(g)
    },
    error = function(e) lastMortSizePlot()
    )

    lastMortSizePlot(p)
    p
  })

  output$mortguildPlot <- renderPlotly({
    req(mortSimData())
    t1 <- max(input$mortyear - 1, 1); t2 <- input$mortyear + 1

    modeGuild <- if (isTRUE(input$triguildToggleMort)) "chosen" else "triple"

    p <- tryCatch({
      ggplotly(
        guildplot(
          mortSimData()$harvested, mortSimData()$unharvested,
          t1, t2,
          guildparams, celticsim,
          mode = modeGuild
        )
      )
    }, error = function(e) lastMortGuildPlot())

    lastMortGuildPlot(p); p
  })

  output$mortdietPlot <- renderPlotly({
    req(mortSimData())
    win  <- list(start = max(input$mortyear - 1, 1),
                 end   = input$mortyear + 1)
    sims <- list(mortSimData()$harvested, mortSimData()$unharvested)

    p <- tryCatch({
      harvest_sub <- lapply(sims, function(sim) {
        sim@n       <- sim@n      [win$start:win$end, , , drop = FALSE]
        sim@n_pp    <- sim@n_pp   [win$start:win$end, ,      drop = FALSE]
        sim@n_other <- sim@n_other[win$start:win$end, ,      drop = FALSE]
        sim
      })

        plotDietCompare(
          harvest_sub,
          species   = input$diet_species_select_mort,
          sim_names = c("Your Sim", "Base Sim")
        )

    },
    error = function(e) {
      lastMortDietPlot()
    })
    lastMortDietPlot(p)
    p
  })



# Fishery Strategy --------------------------------------------------------

# Run code if either Run Simulation or Compare button pressed.
  triggered_button <- reactiveVal(NULL)
  observeEvent(input$goButton2, {
    triggered_button(paste0("goButton2_", Sys.time()))
  })
  observeEvent(input$goButton22, {
    triggered_button(paste0("goButton22_", Sys.time()))
  })

  #changing the timerange to subset on the plot  for yield
  observe({
    time1  <- input$fishyear  + 1
    time22 <- input$fishyear2 + 1

    time12 <- max(input$fishyear2 - 1, 1)
    new_max <- max(time1, time22)

    current <- input$fishyear2_yield
    new_val <- c(
      max(min(current[1], new_max), 0),
      max(min(current[2], new_max), time12)
    )

    updateSliderInput(
      session, "fishyear2_yield",
      max   = new_max,
      value = new_val
    )
  })

  #Sim 1
  setupYearControls(
    input, session,
    sliderId  = "fishyear",
    runBtnId  = "goButton2",
    boxId     = "yearAdjustButtons_fish1",
    resetId   = "resetTimeYear_fish1",
    minusId   = "decYear_fish1",
    plusId    = "incYear_fish1",
    rvName    = "rvFish1"
  )

  #Sim 2
  setupYearControls(
    input, session,
    sliderId  = "fishyear2",
    runBtnId  = "goButton22",
    boxId     = "yearAdjustButtons_fish2",
    resetId   = "resetTimeYear_fish2",
    minusId   = "decYear_fish2",
    plusId    = "incYear_fish2",
    rvName    = "rvFish2"
  )


#Helper function to reactively make new effort vector depending on the input, for any mizer object.
  makeEffort <- function(prefix, gears, base_effort) {
    ef <- base_effort
    for (g in gears) {
      id <- paste0(prefix, g)
      if (!is.null(input[[id]]))
        ef[g] <- input[[id]]
    }
    ef
  }

  #Area to run fishery sim code
  fishSimData <- eventReactive(triggered_button(), {

    gears <- unique(celticsim@gear_params$gear)

    effort1 <- makeEffort("effort_" , gears, celticsim@initial_effort)
    effort2 <- makeEffort("effort2_", gears, celticsim@initial_effort)

    # long enough for *either* slider
    max_year <- max(input$fishyear, input$fishyear2)          # ≥ 0

    pb <- shiny::Progress$new(); on.exit(pb$close())
    pb$set(message = "Running fishery simulation …", value = 0)

    sim1 <- project(
      celticsim, effort = effort1,
      t_max   = max_year * 2 + 2
    )

    pb$inc(0.5)

    btn  <- triggered_button()
    sim2 <- if (startsWith(btn, "goButton22_"))
      project(
        celticsim, effort = effort2,
        t_max   = max_year * 2 + 2
      ) else NULL
    pb$inc(0.5)

    list(sim1   = sim1,
         sim2   = sim2,
         unharv = unfishedprojection)
  })


  #Change time plotted if either sim1 / sim2 panels time range changes.
  lastChange <- reactiveVal("fishyear")
  observeEvent(input$fishyear,  ignoreInit = TRUE, {
    lastChange("fishyear")
  })
  observeEvent(input$fishyear2, ignoreInit = TRUE, {
    lastChange("fishyear2")
  })
  fish_win1 <- reactive({
    y <- if (lastChange() == "fishyear")  input$fishyear  else  input$fishyear2
    list(start = max(y - 1, 1),
         end   = y + 1)
  })

 #

  # Reactive storage for last successful plots
  lastYieldPlot             <- reactiveVal(NULL)
  lastFishSpeciesPlot       <- reactiveVal(NULL)
  lastFishSizePlot          <- reactiveVal(NULL)
  lastFishGuildPlot         <- reactiveVal(NULL)
  lastSpectrumPlot          <- reactiveVal(NULL)
  lastFishDietSinglePlot    <- reactiveVal(NULL)
  lastNutritionPlot         <- reactiveVal(NULL)

  #Plots
  output$yieldPlot <- renderPlotly({
    req(fishSimData())

    sims <- if (is.null(fishSimData()$sim2))
      list(fishSimData()$sim1)               # run-simulation
    else
      list(fishSimData()$sim1,
           fishSimData()$sim2)               # compare

    p <- tryCatch({
      generateYieldDashboard(
        NS_sim          = sims,
        highlight_times = input$fishyear2_yield
      )
    },
    error = function(e) {
      lastYieldPlot()
    })

    lastYieldPlot(p)
    p
  })

  output$fishspeciesPlot <- renderPlotly({
    req(fishSimData())
    win <- fish_win1()

    modeFish <- if (isTRUE(input$triplotToggleFish)) "chosen" else "triple"

    p <- tryCatch({
      ggplotly(
        plotSpeciesWithTimeRange(
          fishSimData()$sim1,
          fishSimData()$unharv,
          win$start, win$end,
          mode = modeFish
        ) +
          scale_x_discrete(limits = ordered_species())
      )
    },
    error = function(e) {
      lastFishSpeciesPlot()
    })

    lastFishSpeciesPlot(p)
    p
  })

  observe({
    if (!is.null(fishSimData()$sim2))
      output$fishspeciesPlot <- renderPlotly({
        win <- fish_win1()

        p <- tryCatch({
          ggplotly(
            plotSpeciesWithTimeRange2(
              fishSimData()$sim1,
              fishSimData()$sim2,
              fishSimData()$unharv,
              win$start, win$end
            ) + scale_x_discrete(limits = ordered_species())
          )
        },
        error = function(e) {
          lastFishSpeciesPlot()
        })

        lastFishSpeciesPlot(p)
        p
      })
  })

  output$fishsizePlot <- renderPlotly({
    req(fishSimData())

    p <- tryCatch({
      if (is.null(fishSimData()$sim2)) {
        g <- plotSpectraRelative(
          fishSimData()$sim1,
          fishSimData()$unharv,
          fish_win1()$start,
          fish_win1()$end
        )
        if (!isTRUE(input$logToggle4)) {
          g <- g + scale_x_continuous()
        }
        ggplotly(g)
      } else {
        g <- plotSpectraRelative2(
          fishSimData()$sim1,
          fishSimData()$unharv,
          fishSimData()$sim2,
          fish_win1()$start,
          fish_win1()$end
        )
        if (!isTRUE(input$logToggle4)) {
          g <- g + scale_x_continuous()
        }
        ggplotly(g)
      }
    }, error = function(e) {
      lastFishSizePlot()
    })

    lastFishSizePlot(p)
    p
  })

  output$fishguildPlot <- renderPlotly({
    req(fishSimData())
    win <- fish_win1()

    modeGuild <- if (isTRUE(input$triguildToggleFish)) "chosen" else "triple"

    p <- tryCatch({
      if (is.null(fishSimData()$sim2)) {
        ggplotly(
          guildplot(
            fishSimData()$sim1, fishSimData()$unharv,
              win$start, win$end,
            guildparams, celticsim,
            mode = modeGuild
          )
        )
      } else {
        ggplotly(
          guildplot_both(
            fishSimData()$sim1, fishSimData()$sim2, fishSimData()$unharv,
            win$start, win$end,
            guildparams, celticsim,
            mode = modeGuild
          )
        )
      }
    },
    error = function(e) {
      lastFishGuildPlot()
    })

    lastFishGuildPlot(p)
    p
  })

  #So that you can repress the button and rerun
  observeEvent(c(input$goButton2, input$goButton22), {
    built(FALSE)
  }, ignoreInit = TRUE)

  #SO that it saves the last plot, and remembers that the plot is already built (so just change data not plot object)
  built            <- reactiveVal(FALSE)
  lastSpectrumPlot <- reactiveVal(NULL)

  #helper to get the data
  getSpectraData <- function(sim, win) {
    df <- plotSpectra(sim$sim1,
                      time_range  = win$start:win$end,
                      return_data = TRUE)
    split(df, df$Species)
  }

  output$spectrumPlot <- renderPlotly({

    input$goButton2; input$goButton22

    p <- tryCatch({

      sim <- isolate(fishSimData()); req(sim)
      win <- isolate(fish_win1())
      if (!is.null(sim$sim2)) {
        df1 <- plotSpectra(sim$sim1,
                           time_range  = win$start:win$end,
                           return_data = TRUE) %>%
          mutate(sim = "Sim 1")
        df2 <- plotSpectra(sim$sim2,
                           time_range  = win$start:win$end,
                           return_data = TRUE) %>%
          mutate(sim = "Sim 2")

        species <- sort(unique(c(df1$Species, df2$Species)))
        maxn <- RColorBrewer::brewer.pal.info["Set3", "maxcolors"]
        if (length(species) <= maxn) {
          colors <- RColorBrewer::brewer.pal(length(species), "Set3")
        } else {
          colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(maxn, "Set3"))(length(species))
        }

        tmp <- plot_ly(source = "spec")
        for (sp in species) {
          i <- which(species == sp)
          sub1 <- df1 %>% filter(Species == sp)
          tmp <- add_lines(
            tmp,
            data        = sub1,
            x           = ~w,
            y           = ~value,
            name        = sp,
            legendgroup = sp,
            line        = list(color = colors[i], dash = "solid"),
            inherit     = FALSE
          )
          sub2 <- df2 %>% filter(Species == sp)
          tmp <- add_lines(
            tmp,
            data        = sub2,
            x           = ~w,
            y           = ~value,
            name        = sp,
            legendgroup = sp,
            line        = list(color = colors[i], dash = "dash"),
            inherit     = FALSE,
            showlegend  = FALSE
          )
        }

        axType <- if (isTRUE(input$logToggle5)) "log" else "linear"
        tmp <- layout(
          tmp,
          xaxis     = list(type = axType, title = "Weight"),
          yaxis     = list(type = "log", title = "Density"),
          hovermode = "closest"
        )
        tmp <- tmp |>
          event_register("plotly_restyle") |>
          event_register("plotly_legendclick") |>
          event_register("plotly_legenddoubleclick")

        lastSpectrumPlot(tmp)
        built(TRUE)
        tmp
      } else {
        spec_df <- plotSpectra(sim$sim1,
                               time_range  = win$start:win$end,
                               return_data = TRUE)
        spec <- split(spec_df, spec_df$Species)

        species <- sort(unique(spec_df$Species))
        maxn <- RColorBrewer::brewer.pal.info["Set3", "maxcolors"]
        if (length(species) <= maxn) {
          colors <- RColorBrewer::brewer.pal(length(species), "Set3")
        } else {
          colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(maxn, "Set3"))(length(species))
        }

        tmp <- plot_ly(source = "spec")
        for (sp in species) {
          i <- which(species == sp)
          sub <- spec[[sp]]
          tmp <- add_lines(
            tmp,
            data    = sub,
            x       = ~w,
            y       = ~value,
            line    = list(color = colors[i]),
            name    = sp,
            inherit = FALSE
          )
        }

        axType <- if (isTRUE(input$logToggle5)) "log" else "linear"
        tmp <- layout(
          tmp,
          xaxis     = list(type = axType, title = "Weight"),
          yaxis     = list(type = "log", title = "Density"),
          hovermode = "closest"
        )
        tmp <- tmp |>
          event_register("plotly_restyle") |>
          event_register("plotly_legendclick") |>
          event_register("plotly_legenddoubleclick")

        lastSpectrumPlot(tmp)
        built(TRUE)
        tmp
      }

    }, error = function(e) {
      message("Spectrum build failed: ", e$message)
      lastSpectrumPlot()
    })

    p
  })

  #Change the time plotted without having to replot everything (thereby saving the legends chosen)
  observeEvent(
    fish_win1(),
    {
      req(built())

      tryCatch({
        win <- fish_win1()
        sim <- fishSimData()

        # ---- pull fresh spectra -------------------------------------------------
        df1 <- plotSpectra(sim$sim1,
                           time_range  = win$start:win$end,
                           return_data = TRUE)
        spec1 <- split(df1, df1$Species)

        if (!is.null(sim$sim2)) {
          df2 <- plotSpectra(sim$sim2,
                             time_range  = win$start:win$end,
                             return_data = TRUE)
          spec2 <- split(df2, df2$Species)
        } else {
          spec2 <- list()
        }

        species <- sort(unique(c(names(spec1), names(spec2))))
        px <- plotlyProxy("spectrumPlot", session)

        i <- 0L
        for (sp in species) {
          if (sp %in% names(spec1)) {
            sub1 <- spec1[[sp]]
            plotlyProxyInvoke(
              px, "restyle",
              list(x = list(sub1$w),
                   y = list(sub1$value)),
              list(i))
            i <- i + 1L
          }
          if (sp %in% names(spec2)) {
            sub2 <- spec2[[sp]]
            plotlyProxyInvoke(
              px, "restyle",
              list(x = list(sub2$w),
                   y = list(sub2$value)),
              list(i))
            i <- i + 1L
          }
        }
      }, error = function(e) {
        message("Spectrum proxy update failed: ", e$message)
      })
    },
    ignoreInit = TRUE
  )

  #Toggle X axis to log or not for Spectrum
  observeEvent(
    input$logToggle5,
    {
      req(built())

      tryCatch({
        axType <- if (isTRUE(input$logToggle5)) "log" else "linear"
        plotlyProxyInvoke(
          plotlyProxy("spectrumPlot", session),
          "relayout",
          list(xaxis = list(type = axType),
               yaxis = list(type = "linear")))
      }, error = function(e) {
        message("Axis relayout failed: ", e$message)
      })
    },
    ignoreInit = TRUE
  )

  output$fishdietsingleplot <- renderPlotly({
    req(fishSimData())

    win <- fish_win1()

    sims <- if (is.null(fishSimData()$sim2))
      list(fishSimData()$sim1)
    else
      list(fishSimData()$sim1,
           fishSimData()$sim2)

    names <- if (length(sims) == 1)
      c("Sim 1")
    else
      c("Sim 1", "Sim 2")

    p <- tryCatch({
      harvest_sub <- lapply(sims, function(p) {
        p@n       <- p@n      [win$start:win$end, , , drop = FALSE]
        p@n_pp    <- p@n_pp   [win$start:win$end, ,      drop = FALSE]
        p@n_other <- p@n_other[win$start:win$end, ,      drop = FALSE]
        p
      })


        plotDietCompare(harvest_sub, species = input$fish_name_select,
                        sim_names = names
        )

    },
    error = function(e) {
      lastFishDietSinglePlot()
    })

    lastFishDietSinglePlot(p)
    p
  })

  output$nutritionplot <- renderPlotly({
    req(fishSimData())

    win <- fish_win1()

    sims <- if (is.null(fishSimData()$sim2))
      list(fishSimData()$sim1)
    else
      list(fishSimData()$sim1,
           fishSimData()$sim2)

    p <- tryCatch({
      ggplotly(
        plotNutrition(sims, fishSimData()$unharv ,win$start:win$end)
      )
    },
    error = function(e) {
      lastNutritionPlot()
    })

    lastNutritionPlot(p)
    p
  })

}




# NOTE - FOR UI, all of the code has tagAppendAttributes, which makes it confusing, but it is necessary
#as you have to label the sections of the code to be able to put it into the tutorial of the app.

ui <- fluidPage(rintrojs::introjsUI(), shinyjs::useShinyjs(),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),

                  tags$script(HTML("
    document.addEventListener('shiny:connected', function() {
      document
        .querySelectorAll('[data-bs-toggle=\"popover\"]')
        .forEach(function(el) {
          new bootstrap.Popover(el, {
            container: 'body',
            html: true,
            sanitize: false
          });
        });
    });
  ")),tags$script(src = "app.js")
                ),
                bslib::page_navbar(
                  id = "bigtabpanel",
                  title = tagList(
                    img(src = "mizer.png", height = "75px",
                        style = "vertical-align: middle; margin-right: 15px; margin-bottom: 5px; margin-top: 5px;"),
                    "mizerShiny"
                  ),
                  selected    = "Single Species",
                  collapsible = TRUE,
                  theme       = bs_theme(bootswatch = "cerulean"),

                  tabPanel(
                    title = "Single Species",

                    tabsetPanel(
                      id       = "mortnsp_tab",
                      selected = "Biomass",

                      tabPanel(
                        title = "Biomass",

                        grid_container(
                          layout    = c("area1 area0"),
                          row_sizes = c("1fr"),
                          col_sizes = c("0.3fr", "1.7fr"),
                          gap_size  = "10px",

                          grid_card(
                            area = "area1",
                            card_body(
                              sliderInput(
                                inputId = "species",
                                label   = HTML(
                                  "Starting Biomass <button id='infoButtonSpecies' class='btn btn-info btn-xs' type='button' \
data-bs-toggle='popover' title='' \
data-bs-content='Slider value indicates the starting biomass of the species. Example: to increase the starting population of a given species by 20%, set value on the slider to 1.2. To decrease by 20%, set value to 0.8.'>\
<strong>?</strong></button>"
                                ),
                                min   = 0,
                                max   = 2,
                                value = 1,
                                step  = 0.01,
                                width = "100%"
                              ) %>% tagAppendAttributes(id = "species_slider"),
                              sliderInput(
                                inputId = "year",
                                label   = "Time Range",
                                min     = 1,
                                max     = sp_max_year,
                                value   = 5,
                                step    = 1,
                                width   = "100%"
                              ) %>% tagAppendAttributes(id = "yearspecies_slider"),
                              shinyjs::hidden(
                                div(id   = "yearAdjustButtons_bio",
                                    style = "display:flex; justify-content:center; gap:10px;",
                                    actionButton("decYear_bio", "-1 year", class = "btn-small"),
                                    actionButton("incYear_bio", "+1 year", class = "btn-small")
                                )
                              ),
                              selectInput(
                                inputId = "species_name_select",
                                label   = "Select a Species:",
                                choices = NULL
                              ) %>% tagAppendAttributes(id = "species_chose"),
                              shinyjs::hidden(
                                actionButton("resetTimeYear_bio", "Reset Time",
                                             class = "btn-small",
                                             style = "color:#2FA4E7;")
                              ),
                              actionButton(inputId = "goButton1",   label = "Run Simulation")
                            )
                          ),

                          grid_card(
                            area = "area0",

                            card_body(                       # added class
                              class = "plot-card",
                              style = "flex: 4; overflow: hidden; margin-top: -0.5rem",
                              tabsetPanel(
                                id = "plotTabs",
                                tabPanel(title = "Species", plotlyOutput("speciesPlot", height = "55vh")),
                                tabPanel(title = "Size",     plotlyOutput("sizePlot",     height = "55vh")),
                                if (app_exists("Including", "guilds_information", "checkGuilds",
                                               "guildparams_preprocessed.Rdata")) {
                                tabPanel(title = "Guilds",   plotlyOutput("guildPlot",    height = "55vh"))},
                                tabPanel(title = "Diet",
                                         div(style = "height:50vh; display:flex;",
                                             plotlyOutput("dietplot", height = "100%", width = "100%")
                                         ))
                              )
                            ),

                            card_body(
                              style = "flex: 1.46;",

                              conditionalPanel(
                                condition = "input.plotTabs == 'Species'",
                                legendUI("infoButtonOrder", legends$biomass_species),
                                br(),
                               # h4(
                                  tagList(
                                    HTML("<span style='position: relative; top:-0.2em; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Species Order on Axis</span>"),
                                    HTML(
                                      "<button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='position:relative; top:-0.25em;' data-bs-toggle='popover' title='' data-bs-content='Select how you want the species to be ordered on the axis. Options include Unordered, Size, and Guild. Click &quot;Custom&quot; to choose an order, it will be saved as &quot;Chosen&quot; in the list.'>
    <strong>?</strong>
  </button>"
                                    ),
                                    actionButton(
                                      "customOrderInfo_bio",
                                      label = HTML("<strong>Custom</strong>"),
                                      class = "btn btn-info btn-xs no-focus-outline",
                                      style = "display:inline-block; position:relative; top:-0.25em; margin-left: 5px;"
                                    )

                                  )
                                #)
                                ,
                                div(id = "species_order_bio_box",
                                selectInput(
                                  inputId = "species_order_bio",
                                  label   = NULL,
                                  choices = c("Unordered", "Size", "Guild", "Chosen")
                                )),
                                div( id = "triMode",
                                materialSwitch(
                                  inputId = "triplotToggle",
                                  label   = HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Chosen Only</span>"),
                                  value   = FALSE,
                                  status  = "info"
                                )
                                )
                              ),

                              conditionalPanel(
                                condition = "input.plotTabs == 'Size'",
                                legendUI("infoButtonOrder", legends$biomass_size),
                                br(),
                                materialSwitch(
                                  inputId = "logToggle",
                                  label   = HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                                  value   = TRUE,
                                  status  = "info"
                                )
                              ),

                              conditionalPanel(
                                condition = "input.plotTabs == 'Guilds'",
                                legendUI("infoButtonOrder", legends$biomass_guild),
                                br(),
                                materialSwitch(
                                  inputId = "triguildToggle",
                                  label   = HTML("<span style='margin-top:0; margin-bottom:0.5rem;
        font-weight:500; color: var(--bs-heading-color);
        line-height:1.2;'>Chosen Only</span>"),
                                  value   = FALSE,
                                  status  = "info"
                                )
                              ),
                              conditionalPanel(
                                condition = "input.plotTabs == 'Diet'",
                                legendUI("infoButtonDietBio", legends$fishery_diet_single),
                                br(),
                                HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Select a Species to Plot</span>"),
                                selectInput(
                                  inputId = "diet_species_select",
                                  label   = NULL,
                                  choices = NULL
                                )
                              )
                            )
                          )
                        )
                      ),

                      tabPanel(
                        title = "Mortality",

                        grid_container(
                          layout    = c("area1 area0"),
                          row_sizes = c("1fr"),
                          col_sizes = c("0.3fr", "1.7fr"),
                          gap_size  = "10px",

                          grid_card(
                            area = "area1",
                            card_body(
                              sliderInput(
                              inputId = "mortspecies",
                              label   = HTML(
                                "Mortality Change <button id='infoButtonMort' class='btn btn-info btn-xs' type='button' \
data-bs-toggle='popover' title='' \
data-bs-content='Slider value indicates the change in mortality of a species. Example: to increase the mortality of a species by 1%, set the value of the slider to 0.01. This will change the mortality throughout the simulation to be 1% higher. If you want it to be a 1% decrease, set value to -0.01'>\
<strong>?</strong></button>"
                              ),
                              min   = -0.25,
                              max   = 0.25,
                              value = 0,
                              step  = 0.001,
                              width = "100%"
                            ) %>% tagAppendAttributes(id = "mort_slider"),
                              sliderInput(
                                inputId = "mortyear",
                                label   = "Time Range",
                                min     = 1,
                                max     = sp_max_year,
                                value   = 5,
                                step    = 1,
                                width   = "100%"
                              ) %>% tagAppendAttributes(id = "yearspecies_slider_mort"),
                              shinyjs::hidden(
                                div(id = "yearAdjustButtons",
                                    style = "display:flex; justify-content:center; gap:10px;",
                                    actionButton("decYear", "-1 year", class = "btn-small"),
                                    actionButton("incYear", "+1 year", class = "btn-small")
                                )
                              ),
                              selectInput(
                                inputId = "name_select",
                                label   = "Select a Species:",
                                choices = NULL
                              ) %>% tagAppendAttributes(id = "species_choose_mort"),
                              shinyjs::hidden(
                                actionButton("resetTimeYear", "Reset Time",
                                             class = "btn-small",                           # drop the "btn-danger"
                                             style = "color:#2FA4E7;")
                              ),
                              actionButton(inputId = "goButton3",       label = "Run Simulation")
                            )
                          ),

                          grid_card(
                            area = "area0",

                            card_body(                       # added class
                              class = "plot-card",
                              style = "flex: 4; overflow: hidden; margin-top: -0.5rem",
                              tabsetPanel(
                                id = "plotTabs_mort",
                                tabPanel(title = "Species", plotlyOutput("mortspeciesPlot", height = "55vh")),
                                tabPanel(title = "Size",    plotlyOutput("mortsizePlot",   height = "55vh")),
                                if (app_exists("Including", "guilds_information", "checkGuilds",
                                               "guildparams_preprocessed.Rdata")) {
                                tabPanel(title = "Guilds",  plotlyOutput("mortguildPlot",  height = "55vh"))},
                                tabPanel(title = "Diet",
                                         div(style = "height:50vh; display:flex;",
                                             plotlyOutput("mortdietPlot", height = "100%", width = "100%")
                                         ))
                              )
                            ),

                            card_body(
                              style = "flex: 1.46;",

                              conditionalPanel(
                                condition = "input.plotTabs_mort == 'Species'",
                                legendUI("infoButtonOrder", legends$mortality_species),
                                br(),
                                  tagList(
                                    HTML("<span style='display:inline-block; position:relative; top:-0.25em; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Species Order on Axis</span>"),

                                    HTML(
                                      "<button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='position:relative; top:-0.25em;' data-bs-toggle='popover' title='' data-bs-content='Select how you want the species to be ordered on the axis. Options include Unordered, Size, and Guild. Click &quot;Custom&quot; to choose an order, it will be saved as &quot;Chosen&quot; in the list.'><strong>?</strong></button>"
                                    ),
                                    actionButton(
                                      "customOrderInfo_mort",
                                      label = HTML("<strong>Custom</strong>"),
                                      class = "btn btn-info btn-xs no-focus-outline",
                                      style = "display:inline-block; position:relative; top:-0.25em; margin-left: 5px;"
                                    )
                                  )
                                ,
                                div(id="species_order_mort_box",
                                selectInput(
                                  inputId = "species_order_mort",
                                  label   = NULL,
                                  choices = c("Unordered", "Size", "Guild", "Chosen")
                                )),
                                div( id = "triMode_mort",
                                materialSwitch(
                                  inputId = "triplotToggleMort",
                                  label   = HTML(
                                    "<span style='margin-top:0; margin-bottom:0.5rem;
     font-weight:500; color: var(--bs-heading-color);
     line-height:1.2;'>Chosen Only</span>"
                                  ),
                                  value   = FALSE,
                                  status  = "info"
                                )
                                )
                              ),

                              conditionalPanel(
                                condition = "input.plotTabs_mort == 'Size'",
                                legendUI("infoButtonOrder", legends$mortality_size)
                                ,
                                br(),
                                materialSwitch(
                                  inputId = "logToggle2",
                                  label   = HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                                  value   = TRUE,
                                  status  = "info"
                                )
                              ),

                              conditionalPanel(
                                condition = "input.plotTabs_mort == 'Guilds'",
                                legendUI("infoButtonOrder", legends$mortality_guild),
                                br(),
                                materialSwitch(
                                  inputId = "triguildToggleMort",
                                  label   = HTML("<span style='margin-top:0; margin-bottom:0.5rem;
        font-weight:500; color: var(--bs-heading-color);
        line-height:1.2;'>Chosen Only</span>"),
                                  value   = FALSE,
                                  status  = "info"
                                )
                              ),
                              conditionalPanel(
                                condition = "input.plotTabs_mort == 'Diet'",
                                legendUI("infoButtonDietMort", legends$fishery_diet_single),
                                br(),
                                HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Select a Species to Plot</span>"),
                                selectInput(
                                  inputId = "diet_species_select_mort",
                                  label   = NULL,
                                  choices = NULL
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    title = "Fishery Strategy",
                    grid_container(
                      layout    = c("area1 area0"),
                      row_sizes = c("1fr"),
                      col_sizes = c("0.3fr", "1.7fr"),
                      gap_size  = "10px",

                      grid_card(
                        area = "area1",
                        card_body(
                          style = "margin-top: -0.5rem",
                          tabsetPanel(
                            tabPanel(
                              title = "Sim 1",
                              sliderInput(
                                inputId = "fishyear",
                                label   = "Time Range",
                                min     = 0,
                                max     = fish_max_year,
                                value   = 5,
                                step    = 1,
                                width   = "100%"
                              ) %>% tagAppendAttributes(id = "fishyyear"),
                              shinyjs::hidden(
                                div(id   = "yearAdjustButtons_fish1",
                                    style = "display:flex; justify-content:center; gap:10px;",
                                    actionButton("decYear_fish1", "-1 year", class = "btn-small"),
                                    actionButton("incYear_fish1", "+1 year", class = "btn-small"),
                                    actionButton("resetTimeYear_fish1", "Reset Time",
                                                 class = "btn-small",
                                                 style = "color:#2FA4E7;")
                                )
                              ),
                              div(id = "fishery_sliders", uiOutput("fishery_sliders_ui")),
                              actionButton(inputId = "goButton2",        label = "Run Simulation")
                            ),
                            tabPanel(
                              title = "Sim 2",
                              sliderInput(
                                inputId = "fishyear2",
                                label   = "Time Range",
                                min     = 0,
                                max     = fish_max_year,
                                value   = 5,
                                step    = 1,
                                width   = "100%"
                              ) %>% tagAppendAttributes(id = "fishyyear"),
                              shinyjs::hidden(
                                div(id   = "yearAdjustButtons_fish2",
                                    style = "display:flex; justify-content:center; gap:10px;",
                                    actionButton("decYear_fish2", "-1 year", class = "btn-small"),
                                    actionButton("incYear_fish2", "+1 year", class = "btn-small"),
                                    actionButton("resetTimeYear_fish2", "Reset Time",
                                                 class = "btn-small",
                                                 style = "color:#2FA4E7;")
                                )
                              ),
                              div(id = "fishery_sliders", uiOutput("fishery_sliders_ui2")),
                              actionButton(inputId = "goButton22",        label = "Compare")
                            )
                          )
                        )
                      ),

                      grid_card(
                        area = "area0",
                        card_body(
                          div(
                            class = "plot-card",
                            style = "flex: 4.5; height:50vh; display:flex; flex-direction:column; overflow: hidden; margin-top: -0.5rem",
                            tabsetPanel(
                              id = "fishy_plots",
                              tabPanel(
                                title = "Species",
                                div(style = "flex:1; display:flex;",
                                    plotlyOutput("fishspeciesPlot", height = "100%", width = "100%")
                                )
                              ),
                              tabPanel(
                                title = "Yield",
                                div(style = "flex:1; display:flex;",
                                    plotlyOutput("yieldPlot", height = "100%", width = "100%")
                                )
                              ),
                              tabPanel(
                                title = "Size",
                                div(style = "flex:1; display:flex;",
                                    plotlyOutput("fishsizePlot", height = "100%", width = "100%")
                                )
                              ),
                              if (app_exists("Including", "guilds_information", "checkGuilds", "guildparams_preprocessed.Rdata")) {
                                tabPanel(
                                  title = "Guild",
                                  div(style = "flex:1; display:flex;",
                                      plotlyOutput("fishguildPlot", height = "100%", width = "100%")
                                  )
                                )
                              },
                              tabPanel(
                                title = "Spectra",
                                div(style = "flex:1; display:flex;",
                                    plotlyOutput("spectrumPlot", height = "100%", width = "100%")
                                )
                              ),
                              tabPanel(
                                title = "Diet",
                                div(style = "height:50vh; display:flex;",
                                    plotlyOutput("fishdietsingleplot", height = "100%", width = "100%")
                                )
                              ),
                              if (app_exists("Including", "Nutrition", "checkNutrition", "nutrition.csv")) {
                                tabPanel(
                                  title = "Nutrition",
                                  div(style = "flex:1; display:flex;",
                                      plotlyOutput("nutritionplot", height = "100%", width = "100%")
                                  )
                                )
                              }
                            )
                          )
                        ),

                        card_body(
                          style = "flex: auto",
                          conditionalPanel(
                            condition = "input.fishy_plots == 'Yield'",
                              div(style = "margin-bottom:1.5rem;",
                                          legendUI("infoButtonOrder", legends$fishery_yield)
                                      ),
                            tags$div(
                              style = "display: flex; align-items: center;",

                              HTML("<span style='margin-top:0px; margin-bottom:0.5rem; margin-right: 0.7em;font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Yield Time Range</span>"),
                                style = "margin: 0 15px 0 0;"
                              ,
                              sliderInput(
                                inputId = "fishyear2_yield",
                                label   = NULL,
                                min     = 1,
                                max     = fish_max_year,
                                value   = c(1, 10),
                                step    = 1,
                                width   = "25%"
                              ) %>% tagAppendAttributes(id = "fishyyear_yield", style = "margin-top: 20px;")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.fishy_plots == 'Species'",
                            legendUI("infoButtonOrder", legends$fishery_species),
                            br(),
                              tagList(
                                HTML("<span style='display:inline-block; position:relative; top:-0.25em; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Species Order on Axis</span>"),
                                HTML(
                                  "<button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='position:relative; top:-0.25em;' data-bs-toggle='popover' title='' data-bs-content='Select how you want the species to be ordered on the axis. Options include Unordered, Size, and Guild. Click &quot;Custom&quot; to choose an order, it will be saved as &quot;Chosen&quot; in the list.'><strong>?</strong></button>"
                                ),
                                actionButton(
                                  "customOrderInfo_fish",
                                  label = HTML("<strong>Custom</strong>"),
                                  class = "btn btn-info btn-xs no-focus-outline",
                                  style = "display:inline-block; position:relative; top:-0.25em; margin-left: 5px;"
                                )
                              ),
                            div(id="species_order_fish_box",
                            selectInput(
                              inputId = "species_order_fish",
                              label   = NULL,
                              choices = c("Unordered", "Size", "Guild", "Chosen")
                            )),
                            div( id = "triMode_fish",
                            materialSwitch(
                              inputId = "triplotToggleFish",
                              label   = HTML(
                                "<span style='margin-top:0; margin-bottom:0.5rem;
     font-weight:500; color: var(--bs-heading-color);
     line-height:1.2;'>Chosen Only</span>"
                              ),
                              value   = FALSE,
                              status  = "info"
                            )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.fishy_plots == 'Size'",
                            legendUI("infoButtonOrder", legends$fishery_size),
                            br(),
                            materialSwitch(
                              inputId = "logToggle4",
                              label   = HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                              value   = TRUE,
                              status  = "info"
                            )
                          ),
                          conditionalPanel(
                            condition = "input.fishy_plots == 'Guild'",
                            legendUI("infoButtonOrder", legends$fishery_guild),
                            br(),
                            materialSwitch(
                              inputId = "triguildToggleFish",
                              label   = HTML("<span style='margin-top:0; margin-bottom:0.5rem;
        font-weight:500; color: var(--bs-heading-color);
        line-height:1.2;'>Chosen Only</span>"),
                              value   = FALSE,
                              status  = "info"
                            )
                          ),
                          conditionalPanel(
                            condition = "input.fishy_plots == 'Spectra'",
                            legendUI("infoButtonOrder", legends$fishery_spectra),
                            br(),
                            materialSwitch(
                              inputId = "logToggle5",
                              label   = HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Log</span>"),
                              value   = TRUE,
                              status  = "info"
                            )
                          ),
                          conditionalPanel(
                            condition = "input.fishy_plots == 'Diet'",
                            legendUI("infoButtonOrder", legends$fishery_diet_single),
                            br(),
                            HTML("<span style='margin-top:0px; margin-bottom:0.5rem; font-weight:500; color: var(--bs-heading-color); line-height:1.2;'>Select a Species to Plot</span>"),
                            selectInput(
                              inputId = "fish_name_select",
                              label   = NULL,
                              choices = NULL

                          )),
                          conditionalPanel(
                            condition = "input.fishy_plots == 'Nutrition'",
                            legendUI("infoButtonOrder", legends$nutrition)
                          )
                        )
                      )
                    )
                  ),
                  bslib::nav_spacer(),
                  bslib::nav_item(
                    actionButton("start_tutorial", "Page Guide", class = "btn btn-primary",
                                 style = "margin-right: 20px; padding: 5px 5px;")
                  )
                )
)

shinyApp(ui = ui, server = server)

