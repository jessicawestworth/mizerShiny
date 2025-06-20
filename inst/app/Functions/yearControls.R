setupYearControls <- function(input, session,
                              sliderId,
                              runBtnId,
                              boxId,
                              resetId,
                              minusId,
                              plusId,rvName) {

  rv <- reactiveValues(
    origMin = NULL,
    origMax = NULL,
    curMax  = NULL
  )

  observeEvent(input[[runBtnId]], ignoreInit = TRUE, {
    if (is.null(rv$origMin)) {
      cfg            <- session$clientData[[ paste0("input_", sliderId, "_min") ]]
      rv$origMin     <- if (length(cfg)) cfg else 1
      cfg            <- session$clientData[[ paste0("input_", sliderId, "_max") ]]
      rv$origMax     <- if (length(cfg)) cfg else 100
    }

    # Set maximum to twice the selected value, capped at 100
    rv$curMax <- max(12, min(2 * input[[sliderId]], 100))

    updateSliderInput(session, sliderId,
                      min   = rv$origMin,
                      max   = rv$curMax,
                      value = min(input[[sliderId]], rv$curMax))

    shinyjs::show(boxId)
    shinyjs::show(resetId)
  })

  # Update maximum value each time the user selects a new value
  observeEvent(input[[sliderId]], ignoreInit = TRUE, {
    # Set maximum to twice the selected value, capped at 100
    rv$curMax <- max(12, min(2 * input[[sliderId]], 100))
    updateSliderInput(session, sliderId, max = rv$curMax)
  })

  observeEvent(input[[plusId]], {
    if (is.null(rv$curMax)) return()
    newVal <- min( input[[sliderId]] + 1 , rv$curMax )
    updateSliderInput(session, sliderId, value = newVal)
  })

  observeEvent(input[[minusId]], {
    if (is.null(rv$origMin)) return()
    newVal <- max( input[[sliderId]] - 1 , rv$origMin )
    updateSliderInput(session, sliderId, value = newVal)
  })

  observeEvent(input[[resetId]], {
    if (is.null(rv$origMin)) return()
    rv$curMax <- rv$origMax
    updateSliderInput(session, sliderId,
                      min   = rv$origMin,
                      max   = rv$origMax,
                      value = rv$origMin)
    shinyjs::hide(boxId)
    shinyjs::hide(resetId)
  })
}
