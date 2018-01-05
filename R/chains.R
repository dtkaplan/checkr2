#' Functions for checking magrittr chains
#' 
#' @export
line_chaining <- function(tidy_code, n = 1L) {
  stopifnot(inherits(tidy_code, "checkr_result"))
  if (failed(tidy_code)) return(tidy_code) # short circuit failure
  success_count <- 0
  for (m in seq_along(tidy_code$code)) {
    if (is_chain(tidy_code$code[[m]])) {
      success_count <- success_count + 1
      if (success_count == n) {
        res <- new_checkr_result(action = "ok", message = "", 
                                 code = list(tidy_code$code[[m]]))
        return(res)
      }
    }
  }
  tidy_code$action = "failed"
  tidy_code$message = 
    if (success_count == 0 ) "Didn't find a chained command."
    else paste("Only", success_count, "chained command(s) found.")
  tidy_code
}

#' @export
chain_element <- function(tidy_code, n = 1L) {
  stopifnot(inherits(tidy_code, "checkr_result"))
  stopifnot(is_chain(tidy_code$code[[1]]))
  
  whole_chain <- tidy_code$code[[1]]
  return(whole_chain)
}

#' @export
is_chain <- function(ex) {
  if (! is.call(ex)) {
    FALSE 
  } else {
    identical(as.name(rlang::lang_head(ex)), as.name("%>%"))
  }
}
