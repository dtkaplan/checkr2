#' Tests on expressions
#'
#' These functions can be applied to EX bindings as produced by `line_where()`.
#' They calculate something about the EX expression.
#'
#' @param ex A straight expression, for instance as bound to `EX` in `line_where().`
#'
#' @export
n_args <- function(ex) {
  length(rlang::lang_args_names(ex))
}
#' @export
func_of <- function(ex) {
  rlang::lang_head(ex)
}
