#' Handles testing given the match to a skeleton pattern
#'
#' @param ex an expression or {}-bracketed set of expressions. This may
#' be produced by `quote()`, or `parse(text = ...)` or some similar language
#' mechanism.
#' @param keys an R statement used for pattern matching and binding, based
#' on the redpen package. This can also be a {}-bracketed set of patterns.
#' @param ... tests to apply to expressions in `ex`. These are typically made
#' with `passif()`, `failif()`, and so on.
#' @param fail a optional character string which sets the message to fail with
#' if the pattern does not match. By default, a non-matching pattern doesn't produce
#' a fail so that evaluation can proceed to subsequent statements.
#'
#' @return a test-result list containing a message string and a directive
#' about whether the expressions in `ex` passed the test.
#'
#' @details The pattern `pat` is applied to each of the expressions in `ex`.
#' The tests are only considered for the first expression in `ex` that matches
#' the pattern.  `passif()` and `failif()` tests, when satisfied, lead to immediate
#' return: no other tests are performed. `noteif()` just adds a note, without
#' terminating the testing.
#'
#'
#' @examples
#' ex <- quote(2+2)
#' wrong1 <- quote(2 - 2)
#' wrong2 <- quote(2*2)
#' if_matches(ex, 2 + 2, passif(TRUE, "carbon copy"))
#' if_matches(ex, `+`(.(a), .(b)), passif(TRUE))
#' if_matches(ex, `+`(.(a), .(b)), passif(a==b, message = "Yes, they are equal!"))
#' if_matches(ex, `+`(.(a), .(b)), passif(a==b, message = "Yes, they are equal! In this case, they are both {{a}}."))
#' if_matches(wrong1, {.(expr); .(f)(.(a), .(b))},
#'   passif(f == `+`, "Right! Addition means {{f}}."),
#'   failif(f != `+`, "In {{expr}}, you used {{f}} instead of +"))
#' if_matches(wrong2, {.(fn)(.(a), .(b)); ..(val)},
#'   noteif(val == 4, "Right overall answer: {{val}}."),
#'   failif(fn != `+`, "You need to use the `+` function, not {{fn}}."),
#'   noteif(val != 4, "The result should be 4, not {{val}}."),
#'   passif(fn == `+` && val == 4 && a == b))
#' if_matches(quote({data(mtcars); plot(mpg ~ hp, data = mtcars)}),
#'   {..(val); .(fn)(.(formula), data = mtcars);}, # note, single . with .(fn)
#'   passif(fn == quote(plot), "You made the plot!"))
#' from_txt <- parse(text = "data(mtcars)\nplot(mpg ~ hp, data = mtcars)")
#' if_matches(from_txt, {..(val); ..(fn)(.(formula), data = mtcars);},
#'   passif(fn == `plot`, "You made the plot!"))


#' @export
if_matches <- function(ex, keys, ...) {
  keys <- node_cadr(enquo(keys))
  # make sure the statements, even if from parse(),
  # are put into the form of a set of bracketed expressions
  keys <- as_bracketed_expressions(keys)
  ex <- as_bracketed_expressions(ex)
  tests <- quos(...)

  # We'll be indexing both <keys> and <ex> from 2, since slot 1 has
  # the bracket `{`

  if (length(keys) <= 1) stop("No expressions given for argument 'keys'.")
  bindings <- list() # outside loop so tests have bindings
                     # for all previous matches.
  for (m in 2:length(ex)) {

    patterns_matched <- rep(FALSE, length(keys)-1)
    for (k in 2:length(keys)) {
      # a formula whose RHS copies the environment
      # that node_match will put in .data
      pattern <- LHS ~ copy_env(.data)
      f_lhs(pattern) <- expr( !! keys[[k]])

      # Grab the list of bindings
      new_bindings <-
        try(node_match(simplify_ex(ex[[m]]), !!pattern),
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
        break;
      } else {
        patterns_matched[k-1] <- TRUE
        new_bindings$. <- NULL
        bindings <- c(bindings, new_bindings)
      }

    }
    if (all(patterns_matched)) {
      # we found a match to expression[[m]] for all the keys
      break;
    }

  }

  # If none of the expressions matched all of the patterns,
  # return now.
  if ( ! all(patterns_matched)) {
    return(new_checkr_result(action = "no pattern match"))
  }

  # run the tests with these bindings
  run_tests(tests, bindings)
  # res <- new_checkr_result()
  # for (k in 1:length(tests)) {
  #   test_data_fun <- eval_tidy(tests[[k]])
  #   the_test <- test_data_fun("test")
  #   # Evaluate the test in the context of the bindings.
  #   if (eval_tidy(the_test, data = bindings)) {
  #     # the test is satisfied.
  #     message = moustache(test_data_fun("message"), bindings)
  #     action = test_data_fun("action")
  #     # Short circuit on pass or fail.
  #     if (action == "note") {
  #       notes <- c(notes, message)
  #     } else if (action %in% c("pass", "fail")) {
  #       res$action <- action
  #       res$message <- message
  #       if (res$action == "fail" && ! is.null(notes))
  #         res$message <- paste(message, "\nNOTE:",
  #                              paste(notes, collapse = "\n"))
  #       return(res)
  #     }
  #   }
  # }
  #
  # res
}


# utility for copying out the bindings defined by redpen pattern
copy_env <- function(E) {
  res <- list()
  nms <- names(E)
  for (nm in nms)
    res[[nm]] <- E[[nm]]

  res
}


# utility for turning the output of parse into a bracketed set of
# expressions
# MAKE THIS INTERNAL eventually
#' @export
as_bracketed_expressions <- function(ex) {
  if (inherits(ex, "character")) ex <- parse(text = ex)
  if (inherits(ex, "expression")) {
    # ex came from the parser, not quote.
    # turn it into a bracketed set of expressions
    Res <- quote({})
    for (k in 1:length(ex))
      Res[[k+1]] <- ex[[k]]
  } else if ( ! inherits(ex, "{")) { # it's a single expression
      # put into the framework of a bracketed set of expressions
      Res <- quote({})
      Res[[2]] <- ex
  } else {
    Res <- ex
  }

  Res

}

# Utility for simplifying expressions that are gratuitously wrapped in
# parentheses.
# NOTE: Any expression like (2+2+2) doesn't need the parens. Expressions
# like (2 + 2)*4 need the parens, but in fact the root of the parse tree will
# be * rather than (.
simplify_ex <- function(ex) {
  if (inherits(ex, "(")) ex[[2]] # the contents of the paren
  else ex
}


