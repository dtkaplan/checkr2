#' test for a misconceived statement
#' 
#' Sometimes you expect a specific code pattern which might reflect a common misconception.
#' `misconception()` let's you mark a pattern as such, causing the test to fail if the pattern 
#' is found.
#' 
#' @param tidy_code A checkr result to use as input
#' @param pattern A code pattern as found by any of the functions returning a checkr_result object.
#' @param message The message to give if the `pattern` is found.
#' 
#' @export
misconception <- function(tidy_code, pattern, message) {
  stopifnot(inherits(tidy_code, "checkr_result"))
  if (failed(tidy_code)) return(tidy_code) # short circuit
  res <- pattern
  if (passed(res) || ok(res)) { # the pattern was found!
    res$action <- "fail"
    res$message <- message
  } else { # the pattern was not found
    # on failure, tests should return a test with code matching the input to the test
    # thus, the code for pattern is to be kept in the result
    return(tidy_code)
  }
  
  res
}