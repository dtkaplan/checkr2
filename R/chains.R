#' Functions for checking magrittr chains
#' 
#' @export
line_chaining <- function(tidy_code, n = 1L) {
  stopifnot(inherits(tidy_code, "checkr_result"))
  if (failed(tidy_code)) return(tidy_code) # short circuit failure
  success_count <- 0
  for (m in seq_along(tidy_code$code)) {
    if (is_chain(tidy_code$code[[m]])) {
      success_count <- success_count + 1
      if (success_count == n) {
        res <- new_checkr_result(action = "ok", message = "", 
                                 code = list(tidy_code$code[[m]]))
        return(res)
      }
    }
  }
  tidy_code$action = "failed"
  tidy_code$message = 
    if (success_count == 0 ) "Didn't find a chained command."
    else paste("Only", success_count, "chained command(s) found.")
  tidy_code
}

# expand a chain into a series of nested function calls with . as the input
#' @export
expand_chain <- function(tidy_code) {
  stopifnot(inherits(tidy_code, "checkr_result"))
  stopifnot(is_chain(tidy_code$code[[1]]))
  if (failed(tidy_code)) return(tidy_code) # short circuit on failure
  CP <- magrittr:::split_chain(rlang::quo_expr(tidy_code$code[[1]]))
  new_code <- list()
  this_env <- environment(tidy_code$code[[1]])
  # handle the extreme lhs
  new_code[[1]] <- rlang::new_quosure(expr = CP$lhs, env = this_env)
  # loop over the remaining elements in the chain
  for (m in seq_along(CP$rhss)) {
    value <- eval_tidy(new_code[[m]]) # the previous element
    this_env <- child_env(.parent = this_env, . = value)
    new_code[[m+1]] <- rlang::new_quosure(expr = CP$rhss[[m]], env = this_env)
  }
  tidy_code$code <- new_code
  return(tidy_code)
}


#' @export
# convert a chain into a list of expressions
chain_elements <- function(ex) {
  if (is_chain(ex)) {
    c(chain_elements(rlang::node_cadr(ex)),
      chain_elements(rlang::node_cddr(ex)))
  } else {
    ex
  }
}

# Given a list of chain elements, turn them back into
# a chain

elements_to_chain <- function(elements) {
  if (length(elements) == 1) return(elements[[1]])
  chain_start <-
    rlang::lang(quote(`%>%`), 
                elements[[1]], 
                elements[[2]])
  for (el in elements[-(1:2)]) {
    chain_start <- rlang::lang(quote(`%>%`),
                               chain_start,
                               el)
  }
  chain_start
}

# evaluate an element of the chain at a given input
eval_chain_element <- function(input, ex) {
  with_input <- rlang::lang(quote(`%>%`),as.name("input"), ex)
  browser()
  eval(with_input)
}


#' #' @export
#' chain_element <- function(tidy_code, n = 1L) {
#'   stopifnot(inherits(tidy_code, "checkr_result"))
#'   stopifnot(is_chain(tidy_code$code[[1]]))
#'   
#'   whole_chain <- tidy_code$code[[1]]
#'   return(whole_chain)
#' }

#' @export
is_chain <- function(ex) {
  if (! is.call(ex)) {
    FALSE 
  } else {
    identical(as.name(rlang::lang_head(ex)), as.name("%>%"))
  }
}
