#' Check for missing statements
#'
#' Match the pattern against all statements in the expressions. If none of the
#' statements matches the pattern, return the message as a fail.
#'
#' @param ex A set of expressions: the submission
#' @param key A pattern to match to the expressions
#' @param message The message to return
#'
#' @examples
#' # check for assignment to a given name
#' USER_CODE <- quote({z <- 1; foo <- z})
#' fail_if_no_match(USER_CODE, x <- ., "You didn't give any value to `x`.")

#' @export
fail_if_no_match <- function(ex, key, message = "Pattern not found.") {
  keys <- rlang::node_cadr(rlang::enquo(key))
  # make sure the statements, even if from parse(),
  # are put into the form of a set of bracketed expressions
  keys <- as_bracketed_expressions(keys)
  ex <- as_bracketed_expressions(ex)

  # We'll be indexing both <keys> and <ex> from 2, since slot 1 has
  # the bracket `{`

  if (length(keys) <= 1) stop("No expressions given for argument 'keys'.")
  patterns_matched <- rep(FALSE, length(ex)-1)
  for (m in 2:length(ex)) {


    for (k in 2:length(keys)) {
      # a formula whose RHS is TRUE
      pattern <- LHS ~ TRUE
      rlang::f_lhs(pattern) <- rlang::expr( !! keys[[k]])

      # Grab the list of bindings
      simp_ex <- simplify_ex(ex[[m]])
      did_it_match <-
        try(redpen::node_match(simp_ex, !!pattern),
            silent = TRUE)

      # If command throws error, special fail on error
      if (inherits(did_it_match, "try-error")) {
        return(
          new_checkr_result(
            action = "Fail on error",
            message = "error evaluating user code" # holds error message
          )
        )
      }
      if (!is.null(did_it_match)) {
        patterns_matched[m-1] <- TRUE
        break; # go to the next expression
      }
    } # end of loop for keys
  } # end of loop for expressions

  # What to return:
  # If none of the expressions matched the pattern, return a fail
  if ( ! any(patterns_matched)) {
    new_checkr_result(action = "fail", message = message)
  } else {
    # otherwise, the expression failed (that is, it found a match)
    # so return a neutral result
    new_checkr_result(action = "no pattern match")
  }

}
