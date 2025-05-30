## ----eval = F-----------------------------------------------------------------
#  
#  mizerShiny(mizerParams = YourParams)
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  mizerShiny(mizerParams = YourParams,
#             baseSpSim = YourSim,
#             baseFishSim = YourSim2)
#  

## ----eval = FALSE-------------------------------------------------------------
#  
#  mizerShiny(mizerParams = YourParams, REPLACE = TRUE)
#  
#  #Then next time you call it, or if you provide the file/package to others
#  #This will run YourParams instead of the default NS_params
#  
#  mizerShiny()
#  
#  #The same will work if you provide mizerSim objects.
#  
#  mizerShiny(mizerParams = YourParams,
#             baseSpSim = YourSim,
#             baseFishSim = YourSim2,
#             REPLACE = TRUE)
#  
#  #Then calling mizerShiny() will load your mizerParams and mizerSim objects
#  #To get back to the NS_params sims, either reinstall the package or repeat the process with NS_params.
#  

## ----eval=FALSE---------------------------------------------------------------
#   output$fishspeciesPlot <- renderPlotly({
#      req(fishSimData())
#  
#      win <- fish_win1()
#  
#      ggplotly(
#        plotSpeciesWithTimeRange(
#          fishSimData()$sim1,
#          fishSimData()$unharv,
#          win$start, win$end
#        ))
#      )
#    })

## ----eval=FALSE---------------------------------------------------------------
#  
#  
#                            tabsetPanel(
#                              id = "fishy_plots",
#                              tabPanel(title = "Species", plotlyOutput("fishspeciesPlot",   height = "100%")),
#                              tabPanel(title = "Yield",   plotlyOutput("yieldPlot",         height = "100%")),
#                              tabPanel(title = "Size",    plotlyOutput("fishsizePlot",      height = "55vh")),
#                              if (app_exists("Including", "guilds_information", "checkGuilds",
#                                             "guildparams_preprocessed.Rdata")) {
#                              tabPanel(title = "Guild",   plotlyOutput("fishguildPlot",     height = "100%"))
#                              },
#                              tabPanel(title = "Spectra", plotlyOutput("spectrumPlot",      height = "100%")),
#                              tabPanel(title = "Diet",    plotlyOutput("fishdietsinglePlot", height = "100%")),
#                              if (app_exists("Including", "Nutrition", "checkNutrition", "nutrition.csv")) {
#                              tabPanel(title = "Nutrition", plotlyOutput("nutritionplot", height = "100%"))
#                              }
#                            )
#  
#  

