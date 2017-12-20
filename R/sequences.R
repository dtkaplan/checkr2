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
#' @examples
#' code <- curly_to_tidy(quote({x <- 2; y <- x^2; z <- x + y}))
#' at(code, Z == "z")
#' at(code, V == 4, Z == "y")
#' at(code, F == `^`)
#' @export
curly_to_tidy <- function(exprs) {
  exprs <- as_bracketed_expressions(exprs)
  # The environment for the first expression
  prev_env <- rlang::caller_env() 
  code <- list()
  values <- list()
  for (m in 2:length(exprs)) {
    next_env <- new.env(parent = prev_env)
    so_far <- try(rlang::eval_bare(exprs[[m]], env = next_env), silent = TRUE)
    if (inherits(so_far, "try-error")) {
      return(checkr_result_on_error(so_far))
    } else {
      values[[m - 1]] <- so_far
    }
    code[[m - 1]] <- new_quosure(exprs[[m]], env = prev_env)
    prev_env <- next_env
  }
  # return a checkr result augmented with the enquosured code and values
  res <- new_checkr_result()
  res$code <- code
  res$values <- values
  
  res
}

# TODO: When evaluating each line, use eval_tidy on the item in the list
# end quo_expr() to get the expression itself
#' @param tidy_code expressions as made by curly_to_tidy()
#' @param ... tests specifying the kind of line we want
#' 
#' @examples
#' tidy_code <- curly_to_tidy(quote({x <- 2; y <- x^3; z <- y + x}))
#' at(tidy_code, F %same_as% quote(`^`))
#'
#' @export
at <- function(tidy_code, ...) {
  n <- matching_line(tidy_code, ...)
  if (n == 0) NULL
  else tidy_code$code[[n]]
}
#' @export
after <- function(tidy_code, ...) {
  n <- matching_line(tidy_code, ...)
  if (n == 0) NULL
  else tidy_code$code[n:length(tidy_code$code)]
}



  
# internal function to run the tests to find a matching line 
matching_line <- function(tidy_code, ...) {
  tests <- rlang::quos(...)
  for (k in 1:length(tidy_code$code)) {
    V <- tidy_code$values[[k]]
    F <- get_function(tidy_code$code[[k]])
    Z <- get_assignment_name(tidy_code$code[[k]])
    EX <- get_
    # other attributes to identify a line?
    
    # run the tests in an environment where V and F 
    # are defined. The tests should return a logical
    passed_all <- TRUE
    for (t in seq_along(tests)) {
      pass_this_test <- 
        eval_tidy(tests[[t]], 
                  data = list(V = V, F = F, Z = Z, `==` = `%same_as%`, `!=` = `%not_same_as%` ))
      if ( ! pass_this_test) {
        passed_all <- FALSE
        break
      }
    }
    # return the line number: zero if no line was found
    if (passed_all) return(k)
  }
  
  0 # no line was found
  
}

# Get the lead function (on the RHS is assignment)
get_function <- function(tidy_expr) {
  ex <- skip_assign(quo_expr(tidy_expr))
  
  if (is_lang(ex)) lang_head(ex)
  else ex
}
# Get the name being assigned to. "" if no assignment.
get_assignment_name <- function(tidy_expr){
  res <- redpen::node_match(tidy_expr, `<-`(.(name), ...) ~ expr_text(name))
  
  if (is.null(res)) ""
  else res
}
