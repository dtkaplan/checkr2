# Functions internal to the checkr2 package.

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
# parentheses and stripping off assignment.
# NOTE: Any expression like (2+2+2) doesn't need the parens. Expressions
# like (2 + 2)*4 need the parens, but the root of the parse tree will
# be * rather than (. So get rid of extraneous parens.
simplify_ex <- function(ex) {
  stopifnot(inherits(ex, "quosure"))
  ex <- skip_assign(ex)
  ex <- rlang::new_quosure(simplify_ex_helper(rlang::quo_expr(ex)),
                           env = environment(ex))
  
  ex
}
simplify_ex_helper <- function(raw_ex) { # recursive to remove nested parens.
  if (inherits(raw_ex, "(")) simplify_ex_helper(raw_ex[[2]])
  else raw_ex
}
