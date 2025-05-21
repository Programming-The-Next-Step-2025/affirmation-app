#' Launch the Affirmation Shiny App
#'
#' This function launches the full Shiny app included in the affirmations package.
#' @export
get_affirmation <- function() {
  app_dir <- system.file("app", package = "affirmations")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

