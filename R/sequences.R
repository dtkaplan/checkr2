#' Translate a sequence of commands into a sequence of quosures
#' 
#' 
#' 
#' 
#' @param exs an expression or expressions in a curly brace
#' 
#' @return a quosure for each expression in exs, with an associated 
#' environment that reflects the state when that expression was evaluated.
#' 
#' 
#' @export
curly_brace_to_quos <- function(exprs) {
  exprs <- as_bracketed_expressions(exprs)
  # The environment for the first expression
  prev_env <- rlang::caller_env() 
  res <- list()
  for (m in 2:length(exprs)) {
    next_env <- new.env(parent = prev_env)
    so_far <- try(rlang::eval_bare(exprs[[m]], env = next_env), silent = TRUE)
    if (inherits(so_far, "try-error")) {
      # don't do anything now. Maybe return a message with 
      # the expression and the error message
    }
    res[[m - 1]] <- new_quosure(exprs[[m]], env = prev_env)
    prev_env <- next_env
  }
  return(res)
}

# TODO: When evaluating each line, use eval_tidy on the item in the list
# end quo_expr() to get the expression itself