#' Comparison operators for bindings
#'
#'
#' @param e1 an expression,
#' @param e2 another expression
#' @export
`%same_as%` <- function(e1, e2) {
  # Handle numbers specially so we don't have to worry about integers and floating points.
  if (is.numeric(e1) && is.numeric(e2)) e1 == e2
  else identical(e1, e2)
}


# Also things like lhs and rhs for formulas, ...
