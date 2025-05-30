# www/legendsTXT.R -------------------------------------------------------------
# Central store for every legend sentence (HTML allowed)

legends <- list(

  biomass_species =
    "Change in biomass of each species across a time range. The X axis shows \
the species and the Y axis is the percentage change in its biomass compared \
with an identical mizer simulation run without the changed starting biomass. \
Each species has three bars (&frac12;&times; time range, chosen, 2&times; time \
range). Red/blue indicate direction; dark &rarr; bright shows \
short/chosen/long time windows.",

  biomass_size =
    "Change in the community size spectrum in comparison to the unchanged \
scenario. The community size spectrum is the biomass of all species in the \
ecosystem at a given size. The blue line is the changed simulation; the grey \
line is the unchanged reference. Use the <em>Log</em> switch to change the \
X&nbsp;axis from log-scale to linear.",

  biomass_guild =
    "Change in feeding guilds across the entire community compared with the \
unchanged model. Each group of three bars represents &frac12;&times;, chosen \
and 2&times; of the selected time range (dark &rarr; bright). Feeding guilds \
group fish by diet and life stage.",


  mortality_species =
    "Change in biomass of each species across a time range after altering \
mortality. Each species has three bars (&frac12;&times;, chosen, 2&times; time \
range). Red/blue indicate direction; dark &rarr; bright shows \
short/chosen/long time windows.",

  fishery_yield =
    "Dashboard of yield (g) for the chosen fishery strategy within the \
selected time range. <strong>Species Change</strong> – yield of individual \
species over time. <strong>Gear Change</strong> – yield of each gear over \
time. <strong>Yield Composition</strong> – total yield for each gear within \
the time range. <strong>Total&nbsp;Yield&nbsp;Per&nbsp;Sim</strong> – species \
composition of total yield (bar height&nbsp;=&nbsp;total yield). \
<strong>Pie Chart</strong> – species composition shown as a pie chart. If two \
simulations are plotted, the second uses dashed lines (line graphs) and its \
own bars/slices (bar and pie plots).",
  fishery_spectra =
    "Biomass density across size for each species. Use the <em>Log</em> switch \
to toggle the X&nbsp;axis between log and linear scales.",

  fishery_diet_single =
    "Diet composition of a selected fish across size.",

  nutrition =
    "Relative change in nutrition compared with the current fishing strategy. \
Calculated from species biomass – higher biomass means higher nutrition values."
)


legends$mortality_size  <- legends$biomass_size
legends$mortality_guild <- legends$biomass_guild
legends$fishery_species <- legends$biomass_species
legends$fishery_size    <- legends$biomass_size
legends$fishery_guild   <- legends$biomass_guild
