#' Tests on expressions
#' 
#' These functions can be applied to EX statements as produced by `chk()`.
#' They calculate something about the expression.
#' 
#' @export
n_args <- function(EX) {
  length(rlang::lang_args_names(EX))
}
#' @export
func_of <- function(EX) {
  rlang::lang_head(EX)
}
