#' Run test statements in the context of the bindings
#'
#' @param test_list a list containing a set of tests
#' @param bindings the bindings resulting from a pattern match to a submission
#'
#' #WE DO NOT need to export this. Just for testing
#' @export
run_tests <- function(test_list, bindings, ex) {
  # run the tests with these bindings
  res <- new_checkr_result()
  notes <- NULL
  # These additions to bindings overwrite == and !=
  bindings[["=="]] <- function(x,y) x %same_as% y
  bindings[["!="]] <- function(x,y) ! x %same_as% y
  for (k in 1:length(test_list)) {
    test <- rlang::eval_tidy(test_list[[k]], data = bindings)
    # will be a function if passif(), failif(), etc. but
    # will be a checkr_result if it's something else
    if (inherits(test, "checkr_result")) {
      # a recursive call to if_matches()
      action  <- test$action
      message <- test$message
    } else {
      # it's a passif(), noteif(), failif()
      the_test <- test("test")
      bindings[["test_string"]] <- rlang::expr_text(quo_expr(the_test))
      bindings[["expression_string"]] <- rlang::expr_text(quo_expr(ex))
      # Evaluate the test in the context of the bindings.
      test_result <- rlang::eval_tidy(the_test, data = bindings)
      message <- moustache(test("message"), bindings)
      action <- test("action", test_result)
    }
    # Short circuit on pass or fail.
    if (action == "note") {
      notes <- c(notes, message) # save notes for later pass or fail
    } else if (action %in% c("pass", "fail")) {
      res$action <- action
      res$message <- message
      if (res$action == "fail" && ! is.null(notes))
        res$message <- paste(message, "\nNOTE:",
                             paste(notes, collapse = "\n"))
      return(res)
    } else if (action %in% "ok") {
        res <- new_checkr_result() # the default: it's OK
    }
  }
  res
}

