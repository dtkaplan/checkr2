#' @title Basic tests for single expressions
#' 
#' @description `assigns` checks whether the expression is an assignment.
#' If target is a character string, additionally checks whether the 
#' target of assignment matches the character string.
#' 
#' These handle just one expression, not a bracketed set.
#' 
#' @return checkr result if no match, 
#' otherwise the rhs of the expression
#' 
#' @param ex a single expression, e.g. a <- sin(x) 
#' @param target the anticipated match
#' 
#' @examples
#' ex1 <- quote(x <- sin(a + b))
#' ex2 <- quote(sin(a + b))
#' assigns(ex1) 
#' assigns(ex2)
#' assigns(ex1, target = FALSE)
#' assigns(ex2, target = FALSE) # shouldn't be assignment
#' assigns(ex1, target = x)
#' assigns(ex1, target = y)

#' @export
assigns <- function(ex, target = TRUE) {
  top <- rlang::lang_head(ex)
  target <- rlang::enquo(target)
  if (is.logical(rlang::quo_expr(target))) {
    target <- rlang::quo_expr(target)
    res <- if (as.name("<-") == top) {
      if (target) rlang::lang_tail(ex)[[2]] # the rhs of ex
      else "shouldn't have been an assignment"
    } else {
      if (target) "should have been assignment"
      else ex # the whole expression
    }
  } else {
    res <- if (as.name("<-") == top && 
      rlang::quo_expr(target) %same_as% lang_tail(ex)[[1]]) {
      rlang::lang_tail(ex)[[2]]
    } else {
      paste0("was supposed to be assignment to '",
            as.character(rlang::quo_expr(target)), "'")
    }
  }
  
  if ( ! is_lang(res))
    res <- new_checkr_result(action = "fail", message = res)
  
  res
}

#' @export
skip_assign <- function(ex) {
  top <- lang_head(ex)
  if (as.name("<-") == top) 
    skip_assign(rlang::lang_tail(ex)[[2]])
  else ex
}

#' @export
has_k_args <- function(ex, k = NULL, more_than = NULL, fewer_than = NULL) {
  ex1 <- skip_assign(ex)
  n <- length(rlang::lang_args(ex1))
  res <- ex
  
  if (! is.null(k)) {
    res <- if (n != k) {
      new_checkr_result(action = "fail", 
                        message = paste(expr_text(ex), "should have had", k, "arguments."))
    } else ex
    return(res)
  }
  if (! is.null(more_than)) {
    res <- if ( ! n > more_than) { new_checkr_result(action = "fail", 
                        message = paste(expr_text(ex), "should have had more than", n, "arguments."))
    
    } else ex
    return(res)
  }
  if (! is.null(fewer_than)) {
    res <- if ( ! n < fewer_than) {
      new_checkr_result(action = "fail", 
                        message = paste(expr_text(ex), "should have had fewer than", n, "arguments."))
    } else ex
    return(res)
  }
  
  res
}

#' Functions that look at the arguments of a
#' matching call, to make sure it meets certain criteria.
#' In the tests, the expression found can be referred to as QQ
#' and its value as VV.
#' 
#' @param ex the expression to check
#' @param nm the name of an argument. Can be bare or back-ticked.
#' @param ... specific tests
#' 
#' @examples
#' ex <- quote(lm(mpg ~ hp, data = mtcars))
#' formula_arg(ex)
#' formula_arg(ex, passif(is_formula(QQ), "Found it!"))
#' formula_arg(ex, failif(f_rhs(QQ) != quote(mpg), "You should have 'mpg' on the right side"))
#' @export
generic_arg <- function(type, type_description, ex, ...) {
  tests <- rlang::quos(...)
  args <- rlang::lang_args(ex)
  target <- NULL # will be the formula, when we find it
  message <- paste(rlang::expr_text(ex), 
                   "doesn't contain an argument that is a", type_description)
  for (k in 1:length(args)) {
    if (type(args[[k]])) {
      target = args[[k]]
      break
    }
  }
  
  res <- if (is.null(target)) { # didn't find it
    new_checkr_result(action = "fail", message = message)
  } else if (length(tests) == 0) {
    new_checkr_result(action = "pass", message = paste("found", type_description))
  } else { # run the tests
    res <- try(
      {bindings <- make_QQ_VV(target)
       run_tests(tests, bindings)})
    checkr_eval_error_result(res)
  }
  
  res
}

#' @export
formula_arg <- function(ex, ...) 
  generic_arg(rlang::is_formula, "formula", ex, ...)
#' @export
data_arg <- function(ex, ...) {
  is_data <- function(arg) {
    if (is.name(arg) || rlang::is_lang(arg)) {
      arg <- try(eval_tidy(arg))
    } 
    inherits(arg, "data.frame" )
  }
  generic_arg(is_data, "data frame", ex, ...)
}
#' @examples
#' ex <- quote(lm(mpg ~ hp, data = mtcars))
#' named_arg(ex, data,  
#'   failif(nrow(VV) <= 10, "data should have more than 10 rows"),
#'   passif(QQ != quote(mtcars), "use the 'mtcars' data frame"))
#' 


#' @export
named_arg <- function(ex, nm, ...) {
  tests <- rlang::quos(...)
  nm <- rlang::enquo(nm)
  nm <- rlang::expr_text(rlang::quo_expr(nm))
  arg_names <- rlang::lang_args_names(ex)
  argv <- rlang::lang_args(ex)
  the_arg <- which(nm == arg_names)
  res <- if (length(the_arg) == 0) {
    new_checkr_result(action = "fail", 
                      message = paste0("could not find an argument named '", nm, "'"))
  } else if (length(tests) == 0) {
    new_checkr_result(action = "note", 
                      message = paste0("found argument named '", nm, "'"))
  } else { # run the tests
    res <- try(
               {bindings <- make_QQ_VV(argv[[the_arg]])
                run_tests(tests, bindings)})
    checkr_eval_error_result(res)
  }
    
  res
}

make_QQ_VV <- function(ex) {
  list(QQ = ex, VV = eval_tidy(ex))
}


checkr_eval_error_result <- function(v) {
  if (inherits(v, "try-error")) {
    message <- attr(v, "condition")
    message <- gsub("^.*\\): ", "", message )
    new_checkr_result(action = "Fail on error",
                      message = paste("You gave an invalid command:", message))
  } else {
    v
  }
}


#' 
#' 
#' 
#' @export
fcall <- function(ex,# the expression
                  assign = NULL,
                  fn = NULL,
                  nargs = NULL, 
                  val = NULL,
                  ...) {
  if (!is.null(assign)) {
    top <- lang_head
   
  } else {
    rhs = ex
  }
  if (!is.null(val)) {
    
  }
  if (!is.null(fn)) {
    
  }
  if (!is.null(nargs)) {
    
  }
}