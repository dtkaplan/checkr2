#' Comparison operators for bindings
#'
#'
#' @param e1 an expression,
#' @param e2 another expression
#' @export
`%same_as%` <- function(e1, e2) {
  identical(e1, e2)
}


# Also things like lhs and rhs for formulas, ...
