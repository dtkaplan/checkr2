#' Pull out an argument calling a specified function
#' 
#' Find an argument
#'
#' @param ex An call to be checked
#' @param n Look for the nth passing argument
#' @param fail character string message on failure
#' @param qfuns A quoted function name or vector of names
#' 
#' @examples
#' ex <- for_checkr(quote(sin(53 * pi / 180)))
#' arg_calling(ex, qfuns = quote(sin))
#'
#' @export
arg_calling <- function(ex, n=1L, fail="", qfuns) {
  test <- function(arg) {
    arg %calls% qfuns
  }
  generic_arg(ex, "call to function", test, n = n, fail = fail, use_value = FALSE)
}

