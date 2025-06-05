
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mizerShiny

The goal of mizerShiny is to provide a frontend for running
[mizer](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12256)
models, so that a wider range of people can analyse mizer outputs. It
intends to remove the barrier of knowledge and allow individuals to
investigate and understand ecosystem-wide trophic interactions.
Furthermore, it intends to allow users to balance yield, sustainability
and nutrition across different fishery strategies.

## Running mizerShiny

You can install mizerShiny and then run the default North Sea mizer
model like so

``` r
devtools::install_github("CefasRepRes/mizerShiny")
library(mizerShiny)
mizerShiny()
```

## Tutorial

Click the `Page Guide` button on the top right to get a basic
explanation of the current page when in the app. Detailed information
can be found in the [Make-Changes](doc/Make-Changes.pdf) vignette.

Generally for each page, change the model on the left configuration
panel, press `Run Simulation` and then analyse the ecosystem impacts on
the right. Each plot is relative to an unchanged simulation.

## Vignettes

There are 2 vignettes attached with this package.
[User-Guide](doc/User-Guide.pdf) details how to use each feature and
what happens code-wise. [Make-Changes](doc/Make-Changes.pdf) details how
to change the mizerParams object loaded and a basic explanation of
adding your own plotting functions.

## Funding

The funding for this project was provided by the Sustainable Management
of UK Marine Resources (SMMR) programme, which is jointly administered
by the Natural Environment Research Council (NERC) and the Economic and
Social Research Council (ESRC).

Grant numbers are NE/V017039/1 (Cefas) and NE/V01708X/1 (University of
York).
