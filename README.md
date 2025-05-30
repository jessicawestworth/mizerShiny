
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mizerShiny

The goal of mizerShiny is to provide a frontend for running mizer
models, so that a wider range of people can analyse mizer outputs. It
intends to remove the barrier of knowledge and allow individuals to
investigate and understand ecosystem-wide trophic interactions.
Furthermore, it intends to allow users to balance yield, sustainability
and nutrition.

## Installation and Loading mizerShiny

You can install the development version of mizerShiny and then run the
default North Sea mizer model like so

``` r
remotes::install_github("CefasRepRes/mizerShiny")
library(mizerShiny)
mizerShiny()
```

## Tutorial

Click the ‘Page Guide’ button on the top right to get a basic
explanation of the page. Detailed information can be found in the
attached ‘User-Guide’ vignette.

For each page, change the model on the left configuration panel, press
‘Run Simulation’ and then analyse the ecosystem impacts on the right.
Each plot is relative to an unchanged, or ‘base’, simulation.

## Vignettes

There are 2 vignettes attached with this package. ‘User-Guide’ details
how to use each feature and what happens code-wise. ‘Make-Changes’
details how to change the mizerParams object loaded and a basic
explanation of adding your own plotting functions.
