#' Looks for a match to patterns.
#'
#' Looks for a match of all the patterns to one of the expressions. If the match is found, the
#' tests (see `...`) are evaluated in order. A pass or a fail causes an immediate
#' termination of the testing and returns that result. If no pass or fail occurs, a neutral result
#' is created so that evaluation can proceed to subsequent statements.
#'
#' If the patterns match some statement in the expressions, then the tests are evaluated
#' using the bindings established in the pattern match.
#'
#' @aliases line_binding line_value test_binding
#'
#'
#' @return A checkr_test object with an action ("pass", "fail", or "ok") and a
#' message to be displayed to the user.
#'
#' @param ex an expression or {}-bracketed set of expressions. This may
#' be produced by `quote()`, or `parse(text = ...)` or some similar language
#' mechanism.
#' @param keys an R statement used for pattern matching and binding, based
#' on the redpen package. This can also be a {}-bracketed set of patterns.
#' @param fail a character string message. By default, the function
#' will return an "ok" checkr_result if the patterns don't match. If
#' fail is not empty, then a "fail" checkr_result will be returned with
#' the value of fail as the message.
#' @param ... tests to apply to expressions in `ex`. These are typically made
#' with `passif()`, `failif()`, `noteif()`, `test_binding()`, and so on.
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
#' ex <- for_checkr(quote(2+2))
#' wrong1 <- for_checkr(quote(2 - 2))
#' wrong2 <- for_checkr(quote(2*2))
#' line_binding(ex, 2 + 2, passif(TRUE, "carbon copy"))
#' line_binding(ex, 3 + 3)
#' line_binding(ex, 3 + 3, passif(TRUE, "not a match"))
#' line_binding(ex, 3 + 3, passif(TRUE, "not a match"), fail = "not a match")
#' line_binding(ex, 2 + ..(y), must(y != 2, "{{expression_string}} wasn't right. Second argument should not be {{y}}"))
#' line_binding(ex, `+`(.(a), .(b)), passif(TRUE))
#' line_binding(ex, `+`(.(a), .(b)),
#'   passif(a==b, message = "Yes, they are equal!"))
#' line_binding(ex, `+`(.(a), .(b)),
#'   passif(a==b,
#'      message = "Yes, they are equal! In this case, they are both {{a}}."))
#' line_binding(wrong1, {.(expr); .(f)(.(a), .(b))},
#'   passif(f == `+`, "Right! Addition means {{f}}."),
#'   failif(f != `+`, "In {{expr}}, you used {{f}} instead of +"))
#' line_binding(wrong2, {.(fn)(.(a), .(b)); ..(val)},
#'   noteif(val == 4, "Right overall answer: {{val}}."),
#'   failif(fn != `+`, "You need to use the `+` function, not {{fn}}."),
#'   noteif(val != 4, "The result should be 4, not {{val}}."),
#'   passif(fn == `+` && val == 4 && a == b))
#' code2 <- for_checkr(quote({data(mtcars); plot(mpg ~ hp, data = mtcars)}))
#' line_binding(code2,
#'   # note, single . with .(fn)
#'   {..(val); .(fn)(.(formula), data = mtcars);},
#'   passif(fn == quote(plot), "You made the plot!"))


#' @export
line_value <- function(tidy_code, ..., fail = "") {
  line_binding(tidy_code, {..(V); .(EX)}, ..., fail = fail)
}

#' @export
line_binding <- function(tidy_code, keys, ..., fail = "") {
  if (inherits(tidy_code, "checkr_result")) {
    if (tidy_code$action == "fail") return(tidy_code)
    else tidy_code <- tidy_code$code
  }
  keys <- rlang::enexpr(keys)
  # make sure the patterns, even if from parse(),
  # are put into the form of a set of bracketed expressions
  keys <- as_bracketed_expressions(keys)
  tests <- rlang::quos(...)

  # We'll be indexing <keys> from 2, since slot 1 has
  # the bracket `{`

  if (length(keys) <= 1) stop("No expressions given for argument 'keys'.")
  bindings <- list() # outside loop so tests have bindings
                     # for all previous matches.
  for (m in seq_along(tidy_code)) {
    patterns_matched <- rep(FALSE, length(keys)-1)
    for (k in 2:length(keys)) {
      # a formula whose RHS copies the environment
      # that node_match will put in .data
      pattern <- LHS ~ copy_env(.data)
      rlang::f_lhs(pattern) <- rlang::expr( !! keys[[k]])

      # Grab the list of bindings
      # Handle either a simple list of quosures or the output of
      # for_checkr()
      simp_ex <- simplify_ex(tidy_code[[m]])
      new_bindings <-
        try(redpen::node_match(simp_ex, !!pattern),
            silent = TRUE)

      # If command throws error, special fail on error
      if (inherits(new_bindings, "try-error")) {
        return(
          new_checkr_result(
            action = "fail",
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
    res <- if (fail == "") new_checkr_result("ok")
    else new_checkr_result("fail", "Patterns didn't match")
    return(res)
  }

  # run the tests with these bindings
  run_tests(tests, bindings, simp_ex)
}

# just an alias for line_binding. Not really needed, but
# it seems nicer to use "test_binding" within a "line_binding"
#' @export
test_binding <- line_binding

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
# like (2 + 2)*4 need the parens, but the root of the parse tree will
# be * rather than (. So get rid of extraneous parens.
simplify_ex <- function(ex) {
  if (rlang::is_quosure(ex)) {
    exx <- rlang::quo_expr(ex)
    res <- if (inherits(exx, "(")) {
      rlang::new_quosure(simplify_ex(exx), env = environment(ex))
    } else {
      ex
    }
    return(res)
  } else if (inherits(ex, "(")) simplify_ex(ex[[2]]) # the contents of the paren
  else ex
}


