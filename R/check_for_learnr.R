#' Interface for checking learnr exercises.
#'
#' When using learnr, the check statements should *NOT* be wrapped with `check().`
#' Instead, put the `if_matches()` functions directly in the check-chunk. The `ex` argument
#' to `if_matches()` should be literally `USER_CODE`.
#'
#' @examples
#' # as it would be called from the learnr system ...
#' check_for_learnr(label = "first", user_code = "sin(pi)",
#'   check_code = 'if_matches(USER_CODE, .(fn)(..(a)), passif(fn %same_as% as.name("sin") && a == pi, message="Right-o!"))')
#'
#'
#' @export
check_for_learnr <-
  function(label=NULL,
         user_code = NULL,
         solution_code = NULL,
         check_code = NULL,
         envir_result = NULL,
         evaluate_result = NULL, ...,
         debug = FALSE) {
  # while debugging
  if(debug) {
    save_file_name <- sprintf("~/Downloads/CheckR/chunk-%s.rds", label)
    saveRDS(list(label = label,
                 user_code = user_code,
                 solution_code = solution_code,
                 check_code = check_code,
                 envir = envir_result,
                 evaluate_result = evaluate_result),
            file = save_file_name)
  }
  # Pre-evaluation checking
  # if (is.null(envir_result)) {
  #   res <- pre_check_code(user_code, check_code)
  #   if (nchar(res) > 0)
  #     return(list(message = res, correct = FALSE))
  #   else
  #     return(TRUE)
  # }

  res <- check(USER_CODE = user_code,
               tests = parse(text = check_code))

  # turn the result into a value suitable for learnr
  list(correct = res$action == "passed",
       message = res$message,
       user_code = user_code,
       type = ifelse(res$action == passsed,
                     "success", "info"),
       location = "prepend"
  )
}
