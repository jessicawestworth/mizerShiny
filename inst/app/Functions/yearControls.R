# ---- Functions/yearControls.R  ------------------------------------------
# one slider, one run-button, one ±-pair, one reset
setupYearControls <- function(input, session,
                              sliderId,
                              runBtnId,
                              boxId,
                              resetId,
                              minusId,
                              plusId,rvName) {
  
  # remember the ORIGINAL range, and the "current max" after a run
  rv <- reactiveValues(
    origMin = NULL,
    origMax = NULL,
    curMax  = NULL           # will change after the user runs the sim
  )
  
  # ------------------------------------------------------------------------
  # when the user clicks “Run simulation”
  observeEvent(input[[runBtnId]], ignoreInit = TRUE, {
    # first time we see the slider -> save its original limits
    if (is.null(rv$origMin)) {
      cfg            <- session$clientData[[ paste0("input_", sliderId, "_min") ]]
      rv$origMin     <- if (length(cfg)) cfg else 1
      cfg            <- session$clientData[[ paste0("input_", sliderId, "_max") ]]
      rv$origMax     <- if (length(cfg)) cfg else 100
    }
    
    rv$curMax <- input[[sliderId]]           # shrink the MAX to current value
    
    updateSliderInput(session, sliderId,
                      min   = rv$origMin,
                      max   = rv$curMax,
                      value = min(input[[sliderId]], rv$curMax))
    
    shinyjs::show(boxId)
    shinyjs::show(resetId)
  })
  
  # ---- +1 / –1 year ------------------------------------------------------
  observeEvent(input[[plusId]], {
    if (is.null(rv$curMax)) return()          # nothing to do until first run
    newVal <- min( input[[sliderId]] + 1 , rv$curMax )
    updateSliderInput(session, sliderId, value = newVal)
  })
  
  observeEvent(input[[minusId]], {
    if (is.null(rv$origMin)) return()
    newVal <- max( input[[sliderId]] - 1 , rv$origMin )
    updateSliderInput(session, sliderId, value = newVal)
  })
  
  # ---- reset -------------------------------------------------------------
  observeEvent(input[[resetId]], {
    if (is.null(rv$origMin)) return()
    rv$curMax <- rv$origMax                  # back to the full range
    updateSliderInput(session, sliderId,
                      min   = rv$origMin,
                      max   = rv$origMax,
                      value = rv$origMin)
    shinyjs::hide(boxId)
    shinyjs::hide(resetId)
  })
}
