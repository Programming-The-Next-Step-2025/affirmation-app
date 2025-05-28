#' Get a Random Affirmation from Any Personality
#'
#' This function randomly selects and returns one affirmation
#' from the full set of affirmations across all personality types
#' in the `personality_data` list.
#'
#' @return A character string containing a randomly selected affirmation.
#' @examples
#' library(affirmations)
#' random_affirmation()
#' @export
random_affirmation <- function() {
  all_affirmations <- unname(unlist(lapply(personality_data, function(x) x$affirmations)))
  sample(all_affirmations, 1)
}






