intro_steps <- list(
  list(element = "#mort_slider", title = "Mortality Slider",
       intro = "The Mortality slider changes the mortality rate of a given species,
across their entire size range. In mizer, species are separated into
size bins, each with their own mortality rate. A size bin contains all the individuals between certain weights.
The value on this slider is multiplied by the mortality rate for each size bin, then this value is added on to the original mortality...
"
  ),
  list(element = "#mort_slider", title = "Mortality Slider",
       intro = "This change is imposed relatively across all sizes of the given species, as smaller individuals of any fish
species will have a higher mortality rate than larger individuals."
  ),
  list(element = "#yearspecies_slider_mort", title = "Time Range Slider",
       intro = "Changing the value of this slider will change the
year that is plotted and will run the simulation for this amount of time. It is useful to look at different time scales,
as the impact of the imposed change may differ and it will show the
oscillatory change in fish populations. The buttons below
will set the year to 3, 6, or 12 years, which can be thought of
as short, medium, and long term time scales."
  ),
  list(element = "#species_choose_mort", title = "Species Selector",
       intro = "Here is where you choose the species you want to investigate.
It is possible to change all the species within the model."
  ),
  list(element = "#goButton3", title = "Run Simulation",
       intro = "Once you have chosen your settings, press this button to run the simulation.
Now, the 'Time Range' slider can be used to change the time plotted, as well as the +1/-1 buttons.
It will not rerun the simulation so it is considerably faster. Use 'Reset Time' to return to normal when running another simulation."
  ),
  list(element = "#species_order_mort_box", title = "Species Order",
       intro = "As some of the graphs show changes in each of the species within the model,
the order that these species are presented may allow for easier observation of
any general patterns. You can change and customise the order using the options here."
  ),
  list(element = "#plotTabs_mort", title="Plots", intro = "Here is where you can navigate between different aspects of the ecosystem.
       Remember that each plot uses plotly, so you can look at specific areas of the plot by selecting with your mouse (then double click to reset),
       remove plot items by clicking them on the legend (double click to remove everything but that one), hover over plot information to get
       more specific values and save the plot by using the picture icon in the top right of the plot."),
  list(element = "#plotTabs_mort .nav-link[data-value='Species']", title = "Species",
       intro = "The first plot presents the percentage change in each of the species. This percentage change is relative to an equal ecosystem,
except without your imposed change on the species. Each species has 3 bars, which indicate the species percentage change on
across a shorter timescale (a half of the chosen time), the chosen timescale and a longer timescale (double the chosen time)..."
  ),
  list(element = "#plotTabs_mort .nav-link[data-value='Species']", title = "Species continued",
       intro = "Plotting all 3 timescales
aids in understanding the oscillatory nature of fish populations, but also gives a greater resolution of the effect of the imposed change."
  ),
  list(element = "#triMode_mort", title = "Change Time Plotted",
       intro = "When changing the time with the +1/-1 buttons, it is difficult to observe changes when 3 timeranges are plotted. Use this toggle to change
from 3 times plotted to only the chosen time plotted."
  ),
  list(element = "#plotTabs_mort .nav-link[data-value='Size']", title = "Size",
       intro = "The next plot shows the relative change in the size spectrum on a community level. Plotting the size spectrum informs
the viewer of the change in community composition, more specifically, how the distribution of fish size has changed."
  ),
  list(element = "#plotTabs_mort .nav-link[data-value='Guilds']", title = "Guilds",
       intro = "The plot here showcases the change in the guilds within the species, which are fish in the ecosystem that share a distinct
feeding pattern in relation to other fish. Observing this plot helps to understand how the trophic dynamics of the ecosystem are changing.
Additionally, the guilds are plotted as 3 bars of a short, chosen and long timescale."
  ),
  list(element = "#plotTabs_mort .nav-link[data-value='Diet']", title = "Diet",
       intro = "This plot shows what a species of fish is eating across its size range. The majority of
a species diet will be the 'Resource' - the 'Resource' is meant to simulate the plankton in a marine ecosystem and acts as the bottom
trophic level, providing energy for the modelled species. The point of this tab is so that you can find the reasons behind the ecosystem
changes that you have observed, it makes the trophic links between species clear, more specifically, what eats what."
       # Added step
  )
)
