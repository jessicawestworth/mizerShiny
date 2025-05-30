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
#' @export
mizerShiny <- function(mizerParams = NULL,
                       baseSpSim   = NULL,
                       baseFishSim = NULL,
                       REPLACE     = FALSE,
                       ...) {

  ## ---- load default params from Including/mizerParam if none given ----
  params_file <- system.file("Including", "mizerParam", "params.rds",
                             package = "mizerShiny")
  if (is.null(mizerParams) && file.exists(params_file)) {
    mizerParams <- readRDS(params_file)
  } else if (is.null(mizerParams)) {
    mizerParams <- mizer::NS_params
  }

  ## ---- helper to save an object in the expected on-disk location -----------
  save_obj <- function(obj, subdir, fname) {
    dir.create(app_path("Including", subdir), recursive = TRUE, showWarnings = FALSE)
    save(obj, file = app_path("Including", subdir, fname), compress = "xz")
  }

  ## ---- set up the working objects exactly as before ------------------------
  celticsim             <- NULL
  unharvestedprojection <- NULL
  unfishedprojection    <- NULL

  # 1. handle MizerParams -----------------------------------------------------
  if (inherits(mizerParams, "MizerParams")) {
    celticsim <- mizerParams
    if (isTRUE(REPLACE))
      save_obj(mizerParams, "mizerParam", "params.rds")
  }

  # 2. handle baseline sims ---------------------------------------------------
  if (inherits(baseSpSim, "MizerSim")) {
    unharvestedprojection <- baseSpSim
    if (isTRUE(REPLACE))
      save_obj(baseSpSim, "mizerSim/Unharvested", "unharvestedprojection.rdata")
  }
  if (inherits(baseFishSim, "MizerSim")) {
    unfishedprojection <- baseFishSim
    if (isTRUE(REPLACE))
      save_obj(baseFishSim, "mizerSim/Unfished", "unfishedprojection.rdata")
  }

  ## ---- make them visible to the app without touching .GlobalEnv ------------
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
  if (file.exists(dev_path))
    return(shiny::runApp(dev_path, ...))

  pkg_app <- system.file("app", "mizerShinyApp.R", package = "mizerShiny")
  if (pkg_app == "")
    stop("Could not find Shiny app; reinstall or rebuild `mizerShiny`.", call. = FALSE)

  shiny::runApp(pkg_app, ...)
}
