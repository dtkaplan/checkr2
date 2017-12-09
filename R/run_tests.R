#' Run test statements in the context of the bindings
#'
#' @param test_list a list containing a set of tests
#' @param bindings the bindings resulting from a pattern match to a submission
#'
#' #WE DO NOT need to export this. Just for testing
#' @export
run_tests <- function(test_list, bindings) {
  # run the tests with these bindings
  res <- new_checkr_result()
  notes <- NULL
  # These additions to bindings overwrite == and !=
  bindings[["=="]] <- function(x,y) x %same_as% y
  bindings[["!="]] <- function(x,y) ! x %same_as% y
  for (k in 1:length(test_list)) {
    test_result <- rlang::eval_tidy(test_list[[k]], data = bindings)
    if (inherits(test_result, "checkr_result")) {
      # a recursive call to if_matches()
      action  <- test_result$action
      message <- test_result$message
    } else {
      # it's a passif(), noteif(), failif()
      the_test <- test_result("test")
      # Evaluate the test in the context of the bindings.
      if (rlang::eval_tidy(the_test, data = bindings)) {
        # the test is satisfied.
        message <- moustache(test_result("message"), bindings)
        action <- test_result("action")
      } else {
        message <- "test did not pass"
        action <- "default"
      }
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
    }
  }
  res
}

