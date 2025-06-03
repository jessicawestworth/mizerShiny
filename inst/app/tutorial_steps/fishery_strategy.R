intro_steps <- list(
  list(
    element = ".nav-tabs li a[data-value='Sim 1']",
    title   = "Single Simulation",
    intro   = "Using this tab plots one fishing strategy"
  ),
  list(
    element = ".nav-tabs li a[data-value='Sim 2']",
    title   = "Compare Simulations",
    intro   = "Using this tab will plot the fishing strateges on Sim1 and Sim2 tab in comparison with eachother."
  ),
  list(element = "#fishyyear", title = "Year Range to Plot",
       intro = "Changing the value of this slider will change the
                 year that is plotted. It is useful to look at different time scales,
                 as the impact of the imposed change may differ and it will show the
                 oscillatory change in fish populations. The buttons below
                 will set the year to 3, 6, or 12 years, which can be thought of
                 as short, medium, and long term management decisions."
  ),
  list(element = "#fishery_sliders", title = "Fishery Sliders",
       intro = "The sliders here change the fishing effort of each fleet. Effort values loaded on start-up of the app are the preset values and are what
       your simulation will be compared to. Fishing effort can be though of as the intensity of fishing and each fleet can be thought of as fishing vessels which
       fish similar species and with similar size selectivity."
  ),
  list(element = "#fishy_plots .nav-link[data-value='Species']", title = "Species Plot",
       intro = "The plot presents the percentage change in each of the species. This percentage change is relative to the current fishing strategy. Each species has 3 bars, which indicate the species percentage change across a shorter timescale (a half of the chosen time), the chosen timescale and a longer timescale (double the chosen time)...
      "
  ),
  list(element = "#fishy_plots .nav-link[data-value='Species']", title = "Species continued",
       intro = "Plotting all 3 timescales
aids in understanding the oscillatory nature of fish populations, but also gives a greater resolution of the effect of the imposed change.
"
  ),
  list(element = "#triMode_fish", title = "Change Time Plotted",
       intro = "When changing the time with the +1/-1 buttons, it is difficult to observe changes when 3 timeranges are plotted. Use this toggle to change
from 3 times plotted to only the chosen time plotted."
       # Added step
  ),
  list(element = "#species_order_fish_box", title = "Order of Species",
       intro = "As some of the graphs show changes in each of the species within the model,
            the order that these species are presented may allow for easier observation of
            any general patterns. You can change and customise the order using the options here."
  ),
  list(element = "#fishy_plots", title="Plots", intro = "Here is where you can navigate between different aspects of the ecosystem.
       Remember that each plot uses plotly, so you can look at specific areas of the plot by selecting with your mouse (then double click to reset),
       remove plot items by clicking them on the legend (double click to remove everything but that one), hover over plot information to get
       more specific values and save the plot by using the picture icon in the top right of the plot."),
  list(element = "#fishy_plots .nav-link[data-value='Yield']", title = "Yield Plot",
       intro = "Dashboard of yield harvested from the simulation. Use the legend to understand what each of the plots mean"
  ),
  list(element = "#fishy_plots .nav-link[data-value='Size']", title = "Relative Size Spectrum Plot",
       intro = "This plot shows the relative change in the size spectrum on a community level. Plotting the size spectrum informs
      the viewer of the change in community composition, more specifically, how the distribution of fish size has changed. If comparing simulations, sim 2 will be a dashed line"
  ),
  list(element = "#fishy_plots .nav-link[data-value='Guild']", title = "Guild Plot",
       intro = "The plot here showcases the change in the guilds within the species, which are fish in the ecosystem that share a distinct
       feeding pattern in relation to other fish. Observing this plot helps to understand how the trophic dynamics of the ecosystem are changing.
      Additionally, the guilds are plotted as 3 bars of a short, chosen and long timescale."
  ),
  list(element = "#fishy_plots .nav-link[data-value='Spectra']", title = "Size Spectrum Plot",
       intro = "This plot is the size spectrum (the biomass distribution across sizes of fish) of each
             species of fish in the model. If comparing simulations, sim 2 will be a dashed line. Select species using the legend to remove them, then use the
       Time Range slider to see how they change over time. "
  ),
  list(element = "#fishy_plots .nav-link[data-value='Diet']", title = "Diet Plot",
       intro = "This plot shows what a species of fish is eating across its size range. The majority of
a species diet will be the 'Resource' - the 'Resource' is meant to simulate the plankton in a marine ecosystem and acts as the bottom
trophic level, providing energy for the modelled species. The point of this tab is so that you can find the reasons behind the ecosystem
changes that you have observed, it makes the trophic links between species clear, more specifically, what eats what."
  ),
    list(element = "#fishy_plots .nav-link[data-value='Nutrition']", title = "Nutrition",
          intro = "The nutritional quality across the species caught is plotted here. It works by taking the relative biomass changes to the
         current fishing strategy and calculating the difference in available nutrition in the catch. Keep in mind that this works by biomass values, so a higher yield will result in greater nutritional availability."
  )
)
