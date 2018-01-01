#' Translate an 'ok' message into a pass or fail
#' 
#' Allows you to force an OK message to be either a pass or fail.
#' 
#' @param test_result The outcome of some checkr test
#' @param pass If `TRUE`, the ok will be turned into a pass. If `FALSE`, a failure.
#' @param message Character string message to include with the transformed result.
#' 
#' @details 
#' Sometimes "ok" test results come with a message of its own. The message given as an argument
#' will be pre-pended to whatever message was with the "ok" test result.
#'
#' @examples
#' CODE <- for_checkr("sin(pi)")
#' L1 <- line_at(CODE, F != sin) # gives an OK, because `fail` was not set.
#' resolve_ok(L1)
#'  
#' 
#' @export
resolve_ok <- function(test_result, pass = TRUE, message = "Good!") {
  if (test_result$action == "ok") {
    test_result$action <- ifelse(pass, "pass", "fail")
    test_result$message <- paste(message, test_result$message)
    test_result
  } else { # leave things as they were
    test_result
  }
}