#' Identify an individual line in a sequence of commands
#'
#' The tests are written in terms of pronouns
#' - F the function at the highest level (but with assignment removed)
#' - Z the name being bound to the line's value by assignment. ("" if no assignment.)
#' - V the value produced by the line.
#' - EX the expression itself (but with assignment removed)
#'
#' @aliases line_at lines_after
#'
#' @param tidy_code expressions as made by for_checkr()
#' @param ... tests specifying the kind of line we want
#' @param fail if a non-empty string, trigger a failure if no matching
#' line is found, with the string as the message.
#' @param type a test to check whether V is a certain type, e.g. `is.numeric`.
#'
#' @details `fail` and `type` are optional. The `fail` argument merely sets the
#' message if no matching line is found. `type` can be used to ensure that the value
#' V is an appropriate kind for the tests specified in ...
#'
#' @examples
#' tidy_code <- for_checkr(quote({x <- 2; y <- x^3; z <- y + x}))
#' line_at(tidy_code, F %same_as% quote(`^`))
#'
#' @rdname sequences
#' @export
line_at <- function(tidy_code, ..., fail = "", type = NULL) {
  res <- matching_line(tidy_code, fail, ..., type = type,
                       type_text = substitute(type))
  if (res$n == 0) {
    if (nchar(fail)) new_checkr_result(action = "fail", message = res$fail)
    else new_checkr_result(action = "ok", message = "didn't find line in at.")
  }
  else {
    # return just the RHS if assignment
    new_checkr_result(action = "ok", message = "",
                      code = list(skip_assign(tidy_code$code[[res$n]])))   }
}
#' Grab the lines after a specified line (which is included)
#' @export
line_after <- function(tidy_code, ..., fail = "", type = NULL) {
  res <- matching_line(tidy_code, fail, ...)
  if (res$n == 0) {
    if (nchar(fail)) new_checkr_result(action = "fail",
                                       message = res$fail,
                                       type = type,
                                       type_text = substitute(type))
    else NULL
  }
  else tidy_code$code[res$n:length(tidy_code$code)]
}


# internal function to run the tests to find a matching line
matching_line <- function(tidy_code, fail, ..., type = NULL, type_text="") {
  tests <- rlang::quos(...)
  type_failure <- "" # a flag
  for (k in 1:length(tidy_code$code)) {
    V <- tidy_code$values[[k]]
    F <- get_function(simplify_ex(tidy_code$code[[k]]))
    Z <- get_assignment_name(tidy_code$code[[k]])
    EX <- skip_assign(tidy_code$code[[k]])
    # other attributes to identify a line?

    # run the tests in an environment where V and F
    # are defined. The tests should return a logical
    passed_all <- TRUE
    for (t in seq_along(tests)) {
      bindings <- list(V = V, F = F, Z = Z, EX = EX, `==` = `%same_as%`, `!=` = `%not_same_as%` )
      # check whether V is the right type of object
      if ( ! (is.null(type) || type(V))) {
        if ( ! is.null(type_failure)) {
          type_failure <- paste("didn't find a line with a value passing",
                                 type_text)
        }
      } else { # run the tests
        type_failure <- NULL # flag saying "don't give a type-failure message

        pass_this_test <-
          try(rlang::eval_tidy(tests[[t]], data = bindings), silent = TRUE)
        if (inherits(pass_this_test, "try-error")) {
          warning("Error in checkr test statement.")
          passed_all <- FALSE
          break
        }
        else if ( ! pass_this_test) {
          passed_all <- FALSE
          break
        }
      }
    }
  }
  #
  # return the line number: zero if no line was found
  if ( ! (is.null(type_failure) || type_failure == ""))
    list(n = 0, fail = type_failure)
  else
    list(n = ifelse(passed_all, k, 0),
         fail = moustache(fail, bindings = bindings))
}

# Get the lead function (ignoring any assignment)
get_function <- function(tidy_expr) {
  ex <- skip_assign(rlang::quo_expr(tidy_expr))

  if (rlang::is_lang(ex)) rlang::lang_head(ex)
  else ex
}
# Get the name being assigned to. "" if no assignment.
get_assignment_name <- function(tidy_expr){
  res <- redpen::node_match(tidy_expr, `<-`(.(name), ...) ~ rlang::expr_text(name))

  if (is.null(res)) ""
  else res
}
# modify the expression to remove assignment.
skip_assign <- function(ex) {
  if ( ! rlang::is_lang(rlang::quo_expr(ex))) return(rlang::quo_expr(ex))
  top <- rlang::lang_head(ex)
  if (as.name("<-") == top)
    skip_assign(
      rlang::new_quosure(rlang::quo_expr(rlang::lang_tail(ex)[[2]]),
                         environment(ex)))
  else
    rlang::new_quosure(rlang::quo_expr(ex), env = environment(ex))
}
