#' Pull out an line, expression, or argument calling a specified function
#'
#' Find an argument
#'
#' @param ex An call to be checked
#' @param ... unquoted function names
#' @param n Look for the nth passing argument
#' @param message character string message on failure
#' @param just_the_fun Flag for internal use.
#'
#' @examples
#' ex <- for_checkr(quote(15 * sin(53 * pi / 180)))
#' ex2 <- for_checkr(quote(sin(3)))
#' line_calling(ex, qfuns = quote(sin))
#' # look everywhere, including the top-level function
#' call_to(ex, qfuns = quote(sin))
#' call_to(ex2, qfuns = quote(sin))
#' # look only in arguments: the top-level function doesn't count
#' arg_calling(ex, qfuns = quote(sin))
#' arg_calling(ex2, qfuns = quote(sin))
#'
#' @export
arg_calling <- function(ex, ..., n=1L, message = "call to function") {
  qfuns <- quos(...)
  qfuns <- lapply(qfuns, FUN = quo_expr)
  test <- function(arg) {
    arg %calls% qfuns
  }
  generic_arg(ex, "specified function", test, n = n,
              message = message,
              use_value = FALSE)
}

#' @export
line_calling <- function(ex, ..., n=1L, message = "Didn't find matching function.", just_the_fun = FALSE) {
  qfuns <- quos(...)
  qfuns <- lapply(qfuns, FUN = quo_expr)
  check_qfuns(qfuns)

  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure

  # Loop over the lines
  for (m in seq_along(ex$code)) {
    this_line <- skip_assign(ex$code[[m]])
    if ( ! inherits(rlang::quo_expr(this_line), "call")) {
      next;
    }
    top_level <- redpen::node_match(this_line, .(fn)(...) ~ fn )
    res <-
      if (is.null(top_level)) { # the expression isn't a call
        next
      } else { # it is a call
        if (c(top_level) %in% c(qfuns)) {
          ex$code <- list(this_line)
          return(ex)
        }
        else {
          arg_calling(this_line, ..., n=n, message = message)
        }
      }
    if (ok(res)) {
      if (!just_the_fun) {
        res$code <- list(ex$code[[m]])
      }
      return(res)
    }
  }

  # If we got here, there wasn't a match
  new_checkr_result(action = "fail", message = message, code = ex$code)
}

#' @export
call_to <- function(ex, ..., n=1L, message="Didn't find specified call to function.") {
  line_calling(ex, ..., n=n, message = message,
               just_the_fun = TRUE)
}


check_qfuns <- function(qfuns) {
  # are they really quoted functions
  what <- unlist(lapply(c(qfuns), FUN = function(x) rlang::is_function(eval(x))))
  if (! all(what)) stop("passing something other than a quoted function")
}
format_qfuns <- function(qfuns) {
  # make sure they are all functions
  check_qfuns(qfuns)
  fnames <- unlist(lapply(qfuns, expr_text))
  nfuns <- length(fnames)
  if (nfuns > 1) {
    paste( paste0(fnames[-nfuns], collapse = ", "),
           "or", fnames[nfuns])
  } else fnames
}

