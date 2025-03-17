library(testthat)

test_that("validate_inputs passes with valid input", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  newdata <- mtcars
  truth <- mtcars$mpg
  expect_silent(validate_inputs(model, newdata, truth))
})

test_that("validate_inputs errors if newdata is not a data frame", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  newdata <- mtcars$wt  # Not a data frame
  truth <- mtcars$mpg
  expect_error(validate_inputs(model, newdata, truth))
})

test_that("validate_inputs errors if truth length does not match newdata", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  newdata <- mtcars
  truth <- mtcars$mpg[-1]  # One element short
  expect_error(validate_inputs(model, newdata, truth))
})

test_that("validate_inputs errors if required variables are missing in newdata", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  newdata <- mtcars[, "wt", drop = FALSE]  # Missing 'hp'
  truth <- mtcars$mpg
  expect_error(validate_inputs(model, newdata, truth))
})
