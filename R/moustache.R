#' evaluate expressions in a string using a particular environment
#' @examples
#' moustache("hello")
#' moustache("Three plus five is {{3+5}}.")
moustache <- function(string, bindings = env_parent()) {
  # pull out all the instances of {{expr}} from the string
  the_moustache <- '\\{\\{.*?\\}\\}' # not greedy
  matches <- unlist(stringr::str_extract_all(string, the_moustache))
  if (length(matches) == 0) return(string)
  # evaluate the contents of the moustache
  expressions <- gsub("\\}\\}", "", gsub("\\{\\{", "", matches))
  for (j in seq_along(expressions)) {
    val <- try(eval(parse(text = expressions[j]), envir = bindings))
    if (inherits(val, "try-error")) {
      # it wasn't a valid expression
      val <- paste0("'{{", expressions[j], "}}' could not be evaluated.")
    }
    string <- gsub(matches[j], deparse(val), string, fixed = TRUE)
  }

  return(string)
}
