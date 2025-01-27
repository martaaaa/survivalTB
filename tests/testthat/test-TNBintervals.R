test_that("TNBintervals works correctly", {
  left <- c(2, 4, 6, 8)
  right <- c(4, 6, 8, NA)

  intervals <- TNBsurvival::TNBintervals(left, right, nboot = 10)

  expect_named(intervals, c("original", "bootstrap"))
  expect_s3_class(intervals$original, "data.frame")
  expect_length(intervals$bootstrap, 10)
})

test_that("TNBintervals - mismatched lengths triggers error", {
  left <- c(2, 4, 6)
  right <- c(4, 6)

  expect_error(
    TNBsurvival::TNBintervals(left, right),
    regexp = "must have the same length"
  )
})

test_that("TNBintervals - non-numeric inputs trigger error", {
  left <- c("a", "b", "c")
  right <- c(2, 4, 6)

  expect_error(
    TNBsurvival::TNBintervals(left, right),
    regexp = "must be numeric vectors"
  )
})

test_that("TNBintervals - left >= right triggers error", {
  left <- c(2, 4, 6)
  right <- c(4, 4, 8)  # O segundo par é (4,4)

  expect_error(
    TNBsurvival::TNBintervals(left, right),
    regexp = "Each value in 'left' must be strictly less"
  )
})

test_that("TNBintervals - bootstrap replications", {
  left <- c(2, 4, 6, 8)
  right <- c(3, 5, 7, 10)

  result <- TNBsurvival::TNBintervals(left, right, nboot = 5)

  # Verifies if 'bootstrap' has 5 elements
  expect_length(result$bootstrap, 5)

  # Verifies if each element of 'bootstrap' is a data.frame
  purrr::walk(result$bootstrap, ~ expect_s3_class(.x, "data.frame"))
})

test_that("TNBintervals - output structure", {
  left <- c(2, 4, 6)
  right <- c(4, 6, 8)

  result <- TNBsurvival::TNBintervals(left, right)

  # columns of the original
  expect_named(result$original, c("left", "right", "weight", "survival"))

  # Verifies 'bootstrap' is null (nboot = 1 by default)
  expect_null(result$bootstrap)
})


