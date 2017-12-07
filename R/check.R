#' Organize pattern matching possibilities
#'
#' Use this to organize pattern matching outside of a system
#' such as learnr. For instance, when testing statements, you can use
#' check.
#'
#'
#' @param ... A set of pattern-matching tests
#' @param USER_CODE will be text or bracketed expressions from the user. This
#' is intended for the interfaces to other systems, e.g. learnr. It will
#' be set automatically by those interfaces.
#' @param tests Again, for interfaces to other systems. This is how they
#' hand off the tests that were setup.
#'
#'
#' @examples
#' ex <- quote(sqrt(16))
#' ex <- quote(3 + 3)
#' check(
#'   if_matches(ex, {..(val); .(fn)(.)},
#'     failif(val != 4, "Wrong result."),
#'     failif(fn %not_same_as% `sqrt`,
#'            "You should be taking square-root, not {{fn}}."),
#'     passif(TRUE, "The square root calculation was correct!")
#'     ),
#'   if_matches(ex, {.(fn)(.(a), .(b))},
#'     failif(a != b, "Arguments should be equal."),
#'     passif(fn %same_as% `+`, "Right!"))
#' )
#' @export
check <- function(..., USER_CODE = NULL, tests = NULL) {
  sequence <-
    if (is.null(tests)) quos(...)
    else tests

  any_matches <- FALSE
  last_passing_test <- NULL
  for (k in 1:length(sequence)) {
    res <- eval_tidy(sequence[[k]])
    if ( ! inherits(res, "checkr_result"))
      stop("Can only accept arguments producing checkr-result objects, e.g. if_matches().")
    if (res$action != "no pattern match") any_matches <- TRUE
    if (res$action %in% c("fail", "Fail on error")) return(res)
    if (res$action == "pass") last_passing_test <- res
  }

  res <-
    if (!is.null(last_passing_test)) return(last_passing_test)
    else if (any_matches) new_checkr_result() #default
    else new_checkr_result(message = "No matches were found.")

  return(res)
}

new_checkr_result <- function(action = "default", message = "") {
  res <- list(action = action, message = message)
  class(res) <- "checkr_result"

  res
}
#' @export
print.checkr_result <- function(x) {
  cat(paste0("Test result ", x$action, " with message \"", x$message, "\"\n"))
}
