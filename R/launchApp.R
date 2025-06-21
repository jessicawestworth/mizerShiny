#' Launch mizerShiny
#'
#' @param params_file Path to a custom params.rds file to use instead of the default
#' @param sp_sim_file Path to a custom unharvestedprojection.rdata file
#' @param fish_sim_file Path to a custom unfishedprojection.rdata file
#' @param REPLACE If TRUE, copy the provided files to replace the defaults permanently
#' @param ... passed on to `shiny::runApp()`
#' @import shiny
#' @import mizer
#' @import ggplot2
#' @import bslib
#' @import plotly
#' @import gridlayout
#' @import thematic
#' @import dplyr
#' @import forcats
#' @import shinyBS
#' @import rintrojs
#' @import patchwork
#' @import here
#' @import sortable
#' @import shinyjs
#' @import shinyWidgets
#'
#' @examples
#' \dontrun{
#' # Run with default parameters
#' mizerShiny()
#'
#' # Run with custom parameters (temporary)
#' mizerShiny(params_file = "path/to/my/params.rds")
#'
#' # Run with custom parameters and replace defaults permanently
#' mizerShiny(params_file = "path/to/my/params.rds", REPLACE = TRUE)
#'}
#' @export
mizerShiny <- function(params_file = NULL,
                       sp_sim_file = NULL,
                       fish_sim_file = NULL,
                       REPLACE = FALSE,
                       ...) {

  ## Locate the app directory
  app_root <- system.file("app", package = "mizerShiny")
  if (app_root == "") {
    stop("Could not find installed 'app' directory. Please reinstall 'mizerShiny'.", call. = FALSE)
  }
  
  ## Copy custom files if provided
  if (!is.null(params_file)) {
    if (!file.exists(params_file)) {
      stop("params_file not found: ", params_file, call. = FALSE)
    }
    
    target_path <- file.path(app_root, "Including", "mizerParam", "params.rds")
    if (REPLACE) {
      # Create directory if it doesn't exist
      dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(params_file, target_path, overwrite = TRUE)
      message("Replaced default params.rds with: ", params_file)
    } else {
      # Create temporary copy for this session
      temp_dir <- tempdir()
      temp_params_path <- file.path(temp_dir, "params.rds")
      file.copy(params_file, temp_params_path, overwrite = TRUE)
      
      # Temporarily replace the file
      original_path <- target_path
      if (file.exists(original_path)) {
        backup_path <- paste0(original_path, ".backup")
        file.copy(original_path, backup_path, overwrite = TRUE)
        on.exit({
          if (file.exists(backup_path)) {
            file.copy(backup_path, original_path, overwrite = TRUE)
            file.remove(backup_path)
          }
        }, add = TRUE)
      }
      
      dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(temp_params_path, target_path, overwrite = TRUE)
    }
  }
  
  if (!is.null(sp_sim_file)) {
    if (!file.exists(sp_sim_file)) {
      stop("sp_sim_file not found: ", sp_sim_file, call. = FALSE)
    }
    
    target_path <- file.path(app_root, "Including", "mizerSim", "Unharvested", "unharvestedprojection.rdata")
    if (REPLACE) {
      dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(sp_sim_file, target_path, overwrite = TRUE)
      message("Replaced default unharvestedprojection.rdata with: ", sp_sim_file)
    }
  }
  
  if (!is.null(fish_sim_file)) {
    if (!file.exists(fish_sim_file)) {
      stop("fish_sim_file not found: ", fish_sim_file, call. = FALSE)
    }
    
    target_path <- file.path(app_root, "Including", "mizerSim", "Unfished", "unfishedprojection.rdata")
    if (REPLACE) {
      dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(fish_sim_file, target_path, overwrite = TRUE)
      message("Replaced default unfishedprojection.rdata with: ", fish_sim_file)
    }
  }

  ## Locate and run the Shiny app
  dev_path <- file.path("inst", "app", "app.R")
  if (file.exists(dev_path)) {
    return(shiny::runApp(dev_path, ...))
  }

  pkg_app <- system.file("app", "app.R", package = "mizerShiny")
  if (pkg_app == "") {
    stop("Could not find Shiny app; reinstall or rebuild `mizerShiny`.", call. = FALSE)
  }

  shiny::runApp(pkg_app, ...)
}
