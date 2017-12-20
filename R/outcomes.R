#' Collate a test expression with a message and a action.
#'
#' These functions are simply a way to associate a message with a test.
#' The test itself will be evaluated in `run_tests()`, not in these functions.
#'
#' @rdname passif
#' @aliases passif failif noteif
#'
#' @usage passif(test, message)
#' @param test an expression written in terms of values found
#' in the pattern-matching bindings
#' @param message a character string containing the message to return.
#'
#' @details The `message` can include components calculated from the bindings.
#' Enclose these in moustaches, e.g. "The `{{fn}}` function is not appropriate for adding."
#' Within a test, the operators `==` and `!=` in a test have been augmented to deal
#' with language objects such as names. They are translated to be equivalent to "\%same_as\%".

generic_test <- function(pass = c("pass", "fail", "note", "ok"),
                         fail = c("pass", "fail", "note", "ok"),
                         default_message = "default test message" ) {
  pass <- match.arg(pass)
  fail <- match.arg(fail)
  function(test, message = default_message) {
    test <- rlang::enquo(test)
    function(task, res = TRUE) {
      if (task == "test") test
      else if (task == "message") message
      else if (task == "action") ifelse(result, pass, fail)
    }
  }
}

#' @param test an expression returning a logical. Can use the bindings
#' defined in the context in the expression.
#' @param message a text string to display if test fails
#'
#' @examples
#' code <- curly_to_tidy(quote({x <- 2; y <- x^2; z <- x + y}))
#' my_line <- at(code, F == `+`)
#' if_matches(my_line, `+`(..(x), ..(y)), req(y == 4, "use 5 for the second argument to +"))

#' @export
req <- function(test, message = paste("{{test_string}} failed.")) {
  test <- rlang::enquo(test)
  function(task, res = TRUE) {
    if (task == "test") test
    else if (task == "message") message
    else if (task == "action") ifelse(res, "ok", "fail")
  }
}

#' @export
passif <- generic_test(pass="pass", fail = "ok", "Good!")
#' @export
failif <- generic_test(pass = "fail", fail = "ok", "Sorry.")
#' @export
noteif <- generic_test(pass = "note", fail = "ok", "Please note ...")


