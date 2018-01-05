context("chains")

test_that("Can identify an expression as a chain ", {
  expect_true(checkr2:::is_chain(quote(3 %>% sqrt)))
  expect_false(checkr2:::is_chain(quote(7)))
  expect_false(checkr2:::is_chain(quote(sin(7))))
  expect_false(checkr2:::is_chain(mtcars))
})

test_that("Can find a chain in a sequence of lines", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets"); mtcars %>% lm(mpg ~ cyl, data = .)}))
  r1 <- line_chaining(CODE)
  expect_true(passed(r1))
  expect_true(length(r1$code) == 1)
  expect_equal(rlang::quo_expr(r1$code[[1]]), quote(lm(mpg ~ hp, data = mtcars)))
  r2 <- line_chaining(CODE, n = 2)
  expect_false(passed(r2))
  expect_true(length(r2$code) == 2) # for this example
})

test_that("Can expand single chains.", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets");
    mtcars %>% lm(mpg ~ cyl, data = .) %>% summary(.)}))
  r1 <- line_chaining(CODE)
  expand_chain(r1)
})

test_that("Can expand the chains in a sequence of lines", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets");
    mtcars %>% lm(mpg ~ cyl, data = .) %>% summary(.)}))
  r1 <- expand_all_chains(CODE)
  expect_equal(length(r1$code), 4)
  expect_equal(quo_expr(r1$code[[2]]), quote(mtcars))
  expect_equal(quo_expr(r1$code[[4]]), quote(summary(.)))
})

test_that("Can handle chains with just 1 link", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets");
    mtcars %>% lm(mpg ~ cyl, data = .) }))
  r1 <- line_chaining(CODE)
  r2 <- arg_calling(r1, lm)
  r3 <- data_arg(r2, lm)
  expect_false(failed(r3))
  expect_equal(quo_expr(r3$code[[1]]), quo(mtcars))
})
