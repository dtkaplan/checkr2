#' Checking fill-in-the-blanks problems
#'
#' @param ex the expression to check, e.g. USER_CODE
#' @param pat the anticipated correct form. Use elements such as `..one..` to
#' define a blank.
#' @param ... tests to apply to expressions in `ex`. These are typically made
#' with `passif()`, `failif()`, and so on.
#'
#' @examples
#' submission <- quote(res <- sqrt(a^2 + b^2))
#' submission2 <- quote(res <- sin(a^2 + b^2))
#' as_posted <- quote({res <- ..fn..(a^2 + b^2)})
#' check_blanks(submission2, !!as_posted,
#'    failif(fn == quote(sin) , "Wrong function"),
#'    passif(fn == quote(sqrt), "Right: the square root!"))
#' as_posted <- quote({res <- ..fn..(`+`(`^`(a, ..exp1..), `^`(b, ..exp2..)))})
#' @export
check_blanks <- function(ex, pat, ...) {
  cmd <- enquo(pat)
  tests <- quos(...)
  pat_str <- deparse(get_expr(cmd))
  blank_names <- unlist(
    str_extract_all(string = pat_str, "\\.{2}[\\._a-zA-Z0-9]+\\.{2}")
  )
  blank_alphanum <- gsub("^..|..$", "", blank_names)

  # Construct a valid pattern to represent pat.
  # Replace names of blanks with properly formed pattern
  for (k in seq_along(blank_names)) {
    properly <- paste0(".(", blank_alphanum[k], ")")
    pat_str <- gsub(blank_names[k], properly, pat_str, fixed = TRUE )
  }
  pat <- as_bracketed_expressions(parse(text = pat_str))[[2]]
  bindings <- grab_bindings(ex, !!pat,
                req_all_patterns = FALSE,
                fail_if_no_match = FALSE)

  run_tests(tests, bindings)
}


one <- quote(hp)
two <- quote(200)
S <- deparse(quote({x <- ..zero..
  mtcars %>% filter(..one.. < ..num..) %>%
  summarise(mpg = mean(mpg))}))

