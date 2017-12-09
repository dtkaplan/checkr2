#' Produce a passing test result with a message.
#' 
#' This is used to guarantee a pass result if no other checking function returns a 
#' pass or fail.
#' 
#' @param message a character string giving the message for success.

#' @export
pass_anyways <- function(message = "Good!") {
  new_checkr_result(action = "pass", message = message)
}