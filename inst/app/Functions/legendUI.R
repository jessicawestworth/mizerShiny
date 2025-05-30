# R/Functions/legendUI.R
legendUI <- function(id, text) {
  h4(
    "Legend ",
    tags$button(
      id               = id,
      class            = "btn btn-info btn-sm legend-btn",
      type             = "button",
      `data-bs-toggle`    = "popover",
      `data-bs-html`      = "true",
      `data-bs-placement` = "right",
      `data-bs-container` = "body",
      `data-bs-content`   = text,
      tags$strong("View")
    )
  )
}
