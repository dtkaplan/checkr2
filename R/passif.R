#' Collate a test expression with a message and a action.
#'
#' These functions are simply a way to associate a message with a test.
#' The test itself will be evaluated in `line_` functions, `arg_` functions, `check()`, etc.
#'
#' @rdname passif
#' @aliases passif failif noteif
#'
#' @param test an expression written in terms of values found
#' in the pattern-matching bindings
#' @param message a character string containing the message to return.
#'
#' @details The `message` can include components calculated from the bindings.
#' Enclose these in moustaches, e.g. "The `{{F}}` function is not appropriate for adding."
#' Within a test, the operators `==` and `!=` in a test have been augmented to deal
#' with language objects such as names. They are translated to be equivalent to "\%same_as\%".
#' You can refer to the expression being tested with `{{expression_string}}` and to the `test` itself as `{{test_string}}`.
#'
#'
#' @examples
#' code <- for_checkr(quote({x <- 2; y <- x^2; z <- x + y}))
#' my_line <- line_where(code, F == `+`)
#' # note: a double negative ... fail and y != 4
#' check_binding(my_line, `+`(..(x), ..(y)), failif(y != 4, "use 4 for the second argument to +"))
#'
#' @rdname passif
#' @export
passif <- generic_test(pass="pass", fail = "ok", "Good!")
#' @rdname passif
#' @export
failif <- generic_test(pass = "fail", fail = "ok", "Sorry.")
#' @rdname passif
#' @export
noteif <- generic_test(pass = "ok", fail = "ok", "Just a note ...")
