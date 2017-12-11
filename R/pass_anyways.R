#' Produce a test result definitively.
#'
#' These are used to guarantee a pass or fail result if no other checking function returns a
#' pass or fail.
#'
#' @param message a character string giving the message for success.
#' @aliases pass_anyways fail_anyways




#' @export
pass_anyways <- function(message = "Good!") {
  new_checkr_result(action = "pass", message = message)
}

#' @export
fail_anyways <- function(message = "Sorry, it's not right.") {
  new_checkr_result(action = "fail", message = message)
}
