
percentdiff <- function(harvested, unharvested) {
  harvested %>%
    left_join(unharvested, by = "Species") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% ("Resource"))
}
