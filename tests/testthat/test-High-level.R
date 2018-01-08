context("High-level testing to expand coverage")

test_that("rep(1:4, each = 3) problem works", {
  bunch_1_a <- function(USER_CODE) {
    code <- for_checkr(USER_CODE)
    res <- line_where(code, Z != "",
                      message = "Remember to store the result under the name `Id`.")
    res <- line_where(res, Z == "Id", message = "Use `Id` for the assignment, not {{Z}}.")

    res <- line_where(res, F == rep, message = "You're supposed to use `rep()`.")
    the_each_arg <- named_arg(res, "each",
                              message = "Look at the help for `rep()` to see what arguments are available to control the pattern of repetition. (These are documented under `...`)")
    t1 <- check(the_each_arg,
                failif(V != 3,
                       "Good use of `each=`, but each = {{V}} is not the right value."))
    if (failed(t1)) return(t1)

    the_vector <- vector_arg(res, message = "Where do you create the elements to be repeated?")
    t2 <- check(the_vector,
                failif( ! identical(V, 1:4),
                "The elements to be repeated are 1 through 4, not {{V}}. Where do you construct the set 1:4 to pass to `rep()`?"))

    t2
  }

  # For testing ...
  ex1 <- quote(c(1,1,1,2,2,2,3,3,3,4,4,4))
  ex1a <- quote(Id <- c(1,1,1,2,2,2,3,3,3,4,4,4))
  ex2 <- "Id <- rep(1:3, each = 3)"
  ex2a <- "Id <- rep(1:3, each = 4)"
  ex2b <- "Id <- rep(3, each = 4)"
  ex3 <- "Id <- rep(1:4, each = 3)"

  r1 <- bunch_1_a(ex1)
  expect_true(failed(r1))
  expect_equal(r1$message, "Remember to store the result under the name `Id`.")
  r2 <- bunch_1_a(ex1a)
  expect_true(failed(r2))
  expect_equal(r2$message, "You're supposed to use `rep()`.")
  r3 <- bunch_1_a(ex2)
  expect_true(failed(r3))
  expect_true(grepl("The elements to be repeated", r3$message))
  r4 <- bunch_1_a(ex2a)
  expect_true(failed(r4))
  expect_true(grepl("Good use of", r4$message))
  r5 <- bunch_1_a(ex3)
  expect_false(failed(r5))
})
