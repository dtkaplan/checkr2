#' Check whether a test passed or failed
#' 
#' @rdname passed
#' @aliases passed failed
#' 
#' @param t a checkr_test_result such as produced by `line_at()``
#'
#' @export
passed <- function(t) {
  stopifnot(inherits(t, "checkr_result")) 
  t$action == "pass"
}
#' @export
failed <- function(t) {
  stopifnot(inherits(t, "checkr_result")) 
  t$action == "fail"
}