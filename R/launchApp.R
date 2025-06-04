#' Launch mizerShiny
#'
#' @param mizerParams an **MizerParams** object to use instead of the default.
#'                    The initial effort here will be the starting values of
#'                    the effort sliders in the *Fishery Strategy* section.
#'                    If the species list in mizerParams matches the species list in
#'                    the downloaded mizerSim objects, it will use them as baseSpSim
#'                    and baseFishSim. So if making alterations to mizerParams whilst
#'                    retaining species list, remember to reload then replace the mizerSim objects.
#' @param baseSpSim   an **MizerSim** object for the *Single-species* baseline
#'                    (stored internally as `unharvestedprojection`). Initial effort in this object is the initial effort used in running simulations in the 'Single Species' section.
#' @param baseFishSim an **MizerSim** object for the *Fishery Strategy*
#'                    baseline (stored as `unfishedprojection`).
#' @param REPLACE     logical. If `TRUE`, the supplied objects overwrite the
#'                    corresponding files inside *Including/* so they become
#'                    the new defaults on future launches.
#' @param ...         passed on to `shiny::runApp()`
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
#' mizerShiny()
#'
#' #Load a new mizerParams object
#' mizerShiny(mizerParams = NS_params)
#'
#' #Load a new mizerSim object
#' simbio <- project(NS_params, effort=2)
#' simfish <- project(NS_params, effort=0)
#'
#' mizerShiny(mizerParams = NS_params, baseSpSim=simbio, baseFishSim=simfish)
#'
#' #If you like this and want to run it quicker, use REPLACE.
#' mizerShiny(mizerParams = NS_params, baseSpSim=simbio, baseFishSim=simfish, REPLACE=TRUE)
#' #Then this will load the same
#' mizerShiny()
#'
#' @export
mizerShiny <- function(mizerParams = NULL,
                       baseSpSim   = NULL,
                       baseFishSim = NULL,
                       REPLACE     = FALSE,
                       ...) {

  ## IS there the location of the files
  app_root <- system.file("app", package = "mizerShiny")
  if (app_root == "") {
    stop("Could not find installed 'app' directory. Please reinstall 'mizerShiny'.", call. = FALSE)
  }
  including_dir <- file.path(app_root, "Including")
  params_path   <- file.path(including_dir, "mizerParam", "params.rds")

  ## Is there a mizerParams object, either loaded in on function or internally.
  if (is.null(mizerParams) && file.exists(params_path)) {
    mizerParams <- readRDS(params_path)
  } else if (is.null(mizerParams)) {
    mizerParams <- mizer::NS_params
  }

  ## helper to save files (for REPLACE)
  save_obj <- function(obj, subdir, fname) {
    dir_to <- file.path(including_dir, subdir)
    dir.create(dir_to, recursive = TRUE, showWarnings = FALSE)
    save(obj, file = file.path(dir_to, fname), compress = "xz")
  }

  celticsim             <- NULL
  unharvestedprojection <- NULL
  unfishedprojection    <- NULL

  # 1. handle MizerParams -----------------------------------------------------
  if (inherits(mizerParams, "MizerParams")) {
    celticsim <- mizerParams
    if (isTRUE(REPLACE)) {
      # overwrite the installed params.rds
      dir.create(dirname(params_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(mizerParams, file = params_path)
    }
  }

  # 2. handle baseline sims ---------------------------------------------------
  if (inherits(baseSpSim, "MizerSim")) {
    unharvestedprojection <- baseSpSim
    if (isTRUE(REPLACE)) {
      save_obj(baseSpSim, "mizerSim/Unharvested", "unharvestedprojection.rdata")
    }
  }
  if (inherits(baseFishSim, "MizerSim")) {
    unfishedprojection <- baseFishSim
    if (isTRUE(REPLACE)) {
      save_obj(baseFishSim, "mizerSim/Unfished", "unfishedprojection.rdata")
    }
  }

  ## ---- make them visible to the app without touching GlobalEnv -------------
  runtime_env <- list2env(
    list(
      celticsim             = celticsim,
      unharvestedprojection = unharvestedprojection,
      unfishedprojection    = unfishedprojection
    ),
    parent = emptyenv()
  )
  attach(runtime_env, name = "mizerShiny_runtime", warn.conflicts = FALSE)
  on.exit(detach("mizerShiny_runtime"), add = TRUE)

  ## ---- locate the Shiny app -------------------------------------------------
  dev_path <- file.path("inst", "app", "mizerShinyApp.R")
  if (file.exists(dev_path)) {
    return(shiny::runApp(dev_path, ...))
  }

  pkg_app <- system.file("app", "mizerShinyApp.R", package = "mizerShiny")
  if (pkg_app == "") {
    stop("Could not find Shiny app; reinstall or rebuild `mizerShiny`.", call. = FALSE)
  }

  shiny::runApp(pkg_app, ...)
}
