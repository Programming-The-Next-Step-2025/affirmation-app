#' Launch the Affirmation Shiny App
#'
#' This function launches the interactive Shiny application included in the
#' `affirmations` package. The app presents users with a fun personality quiz
#' and returns a personalized affirmation based on their responses.
#'
#' This is the main entry point for users who want to interact with the app
#' in a browser interface. It checks for the existence of the app directory
#' within the installed package and then starts the app using `shiny::runApp()`.
#'
#' @return Launches the Shiny app in the default web browser. Does not return a value.
#' @examples
#' # Launch the app (only works if the affirmations package is installed)
#' get_affirmation()
#'
#' @seealso [shiny::runApp()]
#' @export
get_affirmation <- function() {
  app_dir <- system.file("app", package = "affirmations")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}


