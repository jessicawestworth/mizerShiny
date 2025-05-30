#' MizerSim Relative Community Size Spectrum 
#'
#' This function plot the relative community size spectrum between two 
#' mizerSim objects.
#'
#' @param object1 A mizerSim object, this is the sim you are comparing.
#' @param object2 A mizerSim object, this is the sim you are comparing to.
#'
#' @return A community size spectrum - values are the relative abundance
#' at a given size class.
#'
#' @examples
#' # Compare between mizerSim objects differing in fishing strategy.
#' 
#' plotSpectraRelative(harvestedprojection, unharvestedprojection)
#'
#' @export
plotSpectraRelative <- function(object1, object2, time1, time2) {
  
  sf1 <- mizer::plotSpectra(object1, return_data = TRUE, 
                            resource = FALSE, background = FALSE,
                            time_range = time1:time2)
  sf2 <- mizer::plotSpectra(object2, return_data = TRUE, 
                            resource = FALSE, background = FALSE,
                            time_range = time1:time2)
  
  sf <- left_join(sf2, sf1, by = c("w", "Legend")) |>
    group_by(w) |>
    summarise(x = sum(value.x, na.rm = TRUE),
              y = sum(value.y, na.rm = TRUE)) |>
    mutate(rel_diff = 2 * (y - x) / (x + y))
  
  sf <- ggplot() +
    geom_line(data = sf, 
              aes(x = w, y = rel_diff * 100), 
              color = "#2FA4E7") +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "dark grey", linewidth = 0.75) +
    labs(x = "Size (g)", 
         y = "Percentage Change") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          legend.position = "none",
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))+
    #This is limiting the code to the size range we care about in the app.
    xlim(NA, 10000)+
    scale_x_log10()
  
  return(sf)
}
