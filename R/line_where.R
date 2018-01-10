#' Identify an individual line in a sequence of commands
#'
#' The tests are written in terms of pronouns
#' - F the function at the highest level (but with assignment removed)
#' - Z the name being bound to the line's value by assignment. ("" if no assignment.)
#' - V the value produced by the line.
#' - EX the expression itself (but with assignment removed)
#'
#' @aliases line_where lines_after
#'
#' @param tidy_code expressions as made by for_checkr()
#' @param ... tests specifying the kind of line we want
#' @param message A character string to be included as the message in the result. This
#' can have moustaches written in terms of F, Z, V, or EX
#'
#'
#' @return A checkr test result. By default, if the line is found, the result
#' is an "OK", setting the stage for further testing. If no matching line is found, the result is a fail.
#'
#' @examples
#' tidy_code <- for_checkr(quote({x <- 2; y <- x^3; z <- y + x}))
#' line_where(tidy_code, F == "^")
#'
#' @export
line_where <- function(tidy_code, ..., message = "") {
  stopifnot(inherits(tidy_code, "checkr_result"))
  if (failed(tidy_code)) return(tidy_code) # short circuit on failure
  res <- matching_line(tidy_code, ..., message = message)
  if (res$n == 0) {
    the_message <-
      if (nchar(message)) res$message
      else "Didn't find a line passing tests."
      new_checkr_result(action = "fail", message = the_message, code = tidy_code$code)
  }
  else {
    # note: assignment will be included in the returned code.
    new_checkr_result(action = "pass",
                      message = "",
                      code = tidy_code$code[res$n])
    }
}
#' Grab the lines after a specified line (which is included)
#' @export
lines_after <- function(tidy_code, ..., message = "") {
  res <- matching_line(tidy_code, ..., message = "")
  if (res$n == 0) {
    if (nchar(fail)) new_checkr_result(action = "fail",
                                       message = res$message)
    else NULL
  }
  else tidy_code$code[res$n:length(tidy_code$code)]
}


# internal function to run the tests to find a matching line
matching_line <- function(tidy_code, ..., message = "", type = NULL, type_text="") {
  tests <- rlang::quos(...)
  type_failure <- "" # a flag
  for (k in 1:length(tidy_code$code)) {
    # Create the bindings
    V <- if ("values" %in% names(tidy_code)) {
      tidy_code$values[[k]]
    } else {
      rlang::eval_tidy(tidy_code$code[[k]])
    }
    F <- get_function(simplify_ex(tidy_code$code[[k]]))
    Z <- get_assignment_name(tidy_code$code[[k]])
    EX <- skip_assign(tidy_code$code[[k]])
    # other attributes to identify a line?

    # run the tests in an environment where V and F
    # are defined. The tests should return a logical
    passed_all <- TRUE
    for (t in seq_along(tests)) {
      bindings <- list(V = V, F = F, Z = Z, EX = EX, `==` = `%same_as%`, `!=` = `%not_same_as%` )


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
    if (passed_all) break
  }

  # return the line number: zero if no line was found

  list(n = ifelse(passed_all, k, 0),
       message = moustache(message, bindings = bindings))
}
