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

generic_test <- function(result = c("pass", "fail", "note") ) {
  result <- match.arg(result)
  function(test, message = "default message") {
    test <- rlang::enquo(test)
    function(task) {
      if (task == "test") return(test)
      if (task == "message") return(message)
      if (task == "action") return(result)
    }
  }
}


#' @export
passif <- generic_test("pass")
#' @export
failif <- generic_test("fail")
#' @export
noteif <- generic_test("note")


