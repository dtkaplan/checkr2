#' Collate a test expression with a message and a action.
#'
#' These functions are simply a way to associate a message with a test.
#' The test itself will be evaluated in `run_tests()`, not in these functions.
#'
#' @rdname passif
#' @aliases req passif failif noteif
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


#' @param test an expression returning a logical. Can use the bindings
#' defined in the context in the expression.
#' @param message a text string to display if test fails
#'
#' @examples
#' code <- for_checkr(quote({x <- 2; y <- x^2; z <- x + y}))
#' my_line <- line_where(code, F == `+`)
#' if_matches(my_line, `+`(..(x), ..(y)), must(y == 4, "use 5 for the second argument to +"))

#' #' @export
#' must <- function(test, message = paste("{{test_string}} failed.")) {
#'   test <- rlang::enquo(test)
#'   function(task, res) {
#'     if (task == "test") test
#'     else if (task == "message") ifelse(res, "", message)
#'     else if (task == "action") ifelse(res, "ok", "fail")
#'   }
#' }



generic_test <- function(pass = c("pass", "fail", "ok"),
                         fail = c("pass", "fail", "ok"),
                         default_message = "default test message" ) {
  pass <- match.arg(pass)
  fail <- match.arg(fail)
  function(test, message = default_message) {
    test <- rlang::enquo(test)
    function(task, res) {
      if (task == "test") test
      else if (task == "message") ifelse(res, message, "")
      else if (task == "action") ifelse(res, pass, fail)
    }
  }
}

#' @export
passif <- generic_test(pass="pass", fail = "ok", "Good!")
#' @export
failif <- generic_test(pass = "fail", fail = "ok", "Sorry.")
#' @export
noteif <- generic_test(pass = "ok", fail = "ok", "Just a note ...")


