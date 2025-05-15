#' Give a general affirmation
#'
#' Prints a fixed, positive affirmation message.
#'
#' @return A character string is printed to the console.
#' @examples
#' give_affirmation()
#'
#' @export
give_affirmation <- function() {
  print("You are an absolute productive queen today — another day, another slay!")
}



#' Create a personalized affirmation
#'
#' Combines a user’s name and personality to generate a personalized affirmation.
#'
#' @param name A string with the user's name.
#' @param personality A string like "Softie" or "Brat Summer Girl".
#' @details Returns a cheerful affirmation string.
#' @examples
#' create_affirmation("Alex", "Softie")
create_affirmation <- function(name, personality) {
  paste0("Hey ", name, "! As a ", personality, ", you are radiant and unstoppable!")
}


#' @importFrom glue glue
personalize_affirmation <- function(name) {
  glue::glue("{name}, remember: {get_affirmation()}")
}
