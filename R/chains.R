#' Expand a chain into a series of lines, each with `.` as the input.
#'
#' @usage expand_chain(ex)
#' @usage expand_all_chains(ex)
#'
#' A magrittr chain, e.g. `a \\%>\\% f() \\%>\\% g() \\%>\\% h()` is equivalent to a sequence of nested function
#' calls: `h(g(f(a)))`. It's also equivalent to a
#' sequence of statements: `. <- f(a); . <- g(.); h(.)` Expanding a chain means to rewrite it
#' into this last form. Note that `.` is an object name. It's value changes at each statement (except the last, which
#' is the result returned by the chain). By expanding a chain, you can use `checkr` statements to look at individual
#' function calls in the chain.
#'
#' `expand_chain()` expands one chain. `expand_all_chains()` takes a sequence of lines, some of
#' which may be chains, into an equivalent sequence of lines, none of which are chains.
#'
#'
#' @return A `checkr_result` object with one line for each of the functions in the chain.
#'
#' @details A magrittr chain consists of a sequence of function calls. Each function takes as input
#' the output of the function before it. (The first element of the chain may be an object
#' or a function call.) The `expand_` functions transform chains into a sequence of lines. Each such line
#' (except the first) will be a function with at least one of the inputs being denoted `.`. The value
#' of `.` for each line will be the object that is an input to that line.
#'
#' @param ex A `checkr_result` object with just one line of code.
#'
#' @examples
#' code <- for_checkr(quote({x <- 3 %>% sin() %>% cos(); x %>% sqrt() %>% log()}))
#' lineA <- line_chaining(code)
#' expand_chain(lineA)
#' expand_all_chains(code)
#' @rdname chains
#' @export
expand_chain <- function(ex) {
  stopifnot(inherits(ex, "checkr_result"))
  if ( ! is_chain(ex$code[[1]])) return(ex) # already expanded
  if (failed(ex)) return(ex) # short circuit on failure
  CP <- magrittr:::split_chain(rlang::quo_expr(simplify_ex(ex$code[[1]])))
  new_code <- list()
  this_env <- environment(ex$code[[1]])
  # handle the extreme lhs
  new_code[[1]] <- rlang::new_quosure(expr = CP$lhs, env = this_env)
  # loop over the remaining elements in the chain
  for (m in seq_along(CP$rhss)) {
    value <- eval_tidy(new_code[[m]]) # the previous element
    this_env <- child_env(.parent = this_env, . = value)
    new_code[[m+1]] <- rlang::new_quosure(expr = CP$rhss[[m]], env = this_env)
  }
  ex$code <- new_code
  return(ex)
}

#' @rdname chains
#' @export
expand_all_chains <- function(ex) {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure
  newcode <- list()
  for (m in seq_along(ex$code)) {
    expanded <- expand_chain(new_checkr_result(action = "ok", code = ex$code[m]))
    newcode <- c(newcode, expanded$code)
  }
  ex$code <- newcode

  ex
}

#' @rdname chains
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

#' @rdname chains
#' @export
is_chain <- function(ex) {
  ex <- simplify_ex(ex)
  if (! (is.call(ex) && is.call(quo_expr(ex)) )) {
    FALSE
  } else {
    identical(as.name(rlang::lang_head(ex)), as.name("%>%"))
  }
}
