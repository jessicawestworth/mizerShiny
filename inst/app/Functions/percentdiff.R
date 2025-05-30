#' MizerSim Relative Biomass per Species 
#'
#' This calculates the percentage difference between the value of biomass 
#' that are separated by a Species column.
#'
#' @param harvested An array (time x species)
#' @param unharvested An array (time x species), the value you are comparing to.
#'
#' @return A dataframe of Species and Biomass. Biomass gives the percentage
#' difference of the value of biomass between the harvested and unharvested
#' mizerSim objects.
#' 
#'
#' @examples
#' harvested <- getBiomass(NS_sim)
#' unharvested <- getBiomass(NS_sim)
#' percentdiff(harvested, unharvested)
#'
#' @export
percentdiff <- function(harvested, unharvested) {
  harvested %>%
    left_join(unharvested, by = "Species") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% ("Resource"))
}