#' Grab any bindings from a pattern match
#'
#' @param ex the expression to be searched for the patterns
#' @param keys the set of patterns
#' @param req_all_patterns Logical. If `TRUE`, then all patterns must match some
#' expression in ex.
#' @param fail_if_no_match Logical. If `TRUE`, then all expressions must match a pattern. (IS THIS RIGHT?)
#'
#' @examples
#' ex <- quote(plot(mpg ~ wt, data = subset(mtcars, hp < 150)))
#' key <- quote(.(fn)(.(formula), data = .(the_data)))
#' grab_bindings(ex, !!key, fail_if_no_match = FALSE)
#' ex2 <- quote({x <- 1; y <- x^2})
#' keys <- quote({`<-`(x, ..(first)); `<-`(.(var), .(fn)(x, .(exponent)))})
#' grab_bindings(ex2, !!keys, fail_if_no_match = FALSE)
#' grab_bindings(ex2, !!keys, fail_if_no_match = FALSE, req_all_patterns = FALSE)
#'
#' @export
grab_bindings <- function(ex, keys,
                          req_all_patterns = TRUE,
                          fail_if_no_match = TRUE) {
  keys <- rlang::node_cadr(rlang::enquo(keys))
  # make sure the statements, even if from parse(),
  # are put into the form of a set of bracketed expressions
  keys <- as_bracketed_expressions(keys)
  ex <- as_bracketed_expressions(ex)
  bindings <- list()

  if (length(keys) <= 1) stop("No expressions given for argument 'keys'.")
  for (m in 2:length(ex)) {
    patterns_matched <- rep(FALSE, length(keys)-1)

    for (k in 2:length(keys)) {
      # a formula whose RHS copies the environment
      # that node_match will put in .data
      pattern <- LHS ~ copy_env(.data)
      rlang::f_lhs(pattern) <- rlang::expr( !! keys[[k]])

      # Grab the list of bindings
      new_bindings <-
        try(redpen::node_match(simplify_ex(ex[[m]]), !!pattern),
            silent = TRUE)

      # If command throws error, special fail on error
      if (inherits(new_bindings, "try-error")) {
        return(
          new_checkr_result(
            action = "Fail on error",
            message = as.character(new_bindings) # holds error message
          )
        )
      }

      if (is.null(new_bindings)) {
        if (fail_if_no_match) break;
      } else {
        patterns_matched[k-1] <- TRUE
        new_bindings$. <- NULL
        bindings <- c(bindings, new_bindings)
      }
    }

  }
  if (req_all_patterns && ! all(patterns_matched)) {
    return(new_checkr_result(action = "no pattern match"))
  }
  bindings
}
