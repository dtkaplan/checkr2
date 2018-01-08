#' Apply passif/failif/okif tests to an identified code object
#'
#' A generic testing function for applying passif/failif tests with V and EX bindings
#' to a checkr_result object. Unlike `line_where()`, other line functions, no patterns
#' need to be provided. Just the passif/failif tests.
#'
#' @param ex the checkr_result object with a single line of code
#' @param ... passif/failif tests to be applied
#'
#' @return a checkr_result object reflecting the outcome of the tests
#'
#' @export
check <- function(tidy_code_line, ..., message = "Sorry!") {
  stopifnot(inherits(tidy_code_line, "checkr_result"),
            length(tidy_code_line$code) == 1)
  line_binding(tidy_code_line, {.(EX); ..(V)}, ..., message = message)
}
