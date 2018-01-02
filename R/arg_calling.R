#' Pull out an line, expression, or argument calling a specified function
#'
#' Find an argument
#'
#' @param ex An call to be checked
#' @param n Look for the nth passing argument
#' @param fail character string message on failure
#' @param pass character string message and flag meaning success is a pass, not just an OK.
#' @param qfuns A quoted function name or vector of names
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
arg_calling <- function(ex, n=1L, fail="", pass="", qfuns) {
  check_qfuns(qfuns)
  test <- function(arg) {
    arg %calls% qfuns
  }
  generic_arg(ex, "call to function", test, n = n,
              fail = fail, pass = pass,
              use_value = FALSE)
}

#' @export
line_calling <- function(ex, n=1L, fail = "", pass = "", qfuns, just_the_fun = FALSE) {
  check_qfuns(qfuns)
  # is it at the top level?
  if ( ! inherits(quo_expr(ex$code[[1]]), "call")) {
    return(new_checkr_result(action = "fail", message = fail))
  }
  top_level <- redpen::node_match(ex$code[[1]], .(fn)(...) ~ fn )
  res <- if (is.null(top_level)) { # the expression isn't a call
    return(new_checkr_result(action = "fail", message = fail))
  } else { # it is a call
    if (c(top_level) %in% c(qfuns)) ex
    else arg_calling(ex, n=n, fail = fail, pass = pass, qfuns)
  }
  if (ok(res) && ! just_the_fun) res$code <- ex$code

  res
}

#' @export
call_to <- function(ex, n=1L, fail="", pass="", qfuns) {
  line_calling(ex, n=n, fail = fail, pass = pass,
               qfuns, just_the_fun = TRUE)
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

