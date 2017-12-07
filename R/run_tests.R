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
    test_data_fun <- eval_tidy(test_list[[k]])
    the_test <- test_data_fun("test")
    # Evaluate the test in the context of the bindings.
    if (eval_tidy(the_test, data = bindings)) {
      # the test is satisfied.
      message = moustache(test_data_fun("message"), bindings)
      action = test_data_fun("action")
      # Short circuit on pass or fail.
      if (action == "note") {
        notes <- c(notes, message)
      } else if (action %in% c("pass", "fail")) {
        res$action <- action
        res$message <- message
        if (res$action == "fail" && ! is.null(notes))
          res$message <- paste(message, "\nNOTE:",
                               paste(notes, collapse = "\n"))
        return(res)
      }
    }
  }
  
  res
}