test_that("single.TNBsurvival works correctly", {
  data <- data.frame(
    left = c(2, 4, 6),
    right = c(4, 6, NA),
    weight = c(0.3, 0.5, 0.2),
    survival = c(1.0, 0.7, 0.2)
  )
  times <- c(1, 3, 5, 7)
  result <- survivalTB::single.TNBsurvival(data, times)
  expect_type(result, "double")
  expect_length(result, length(times))
})

test_that("single.TNBsurvival - times out of range", {
  data <- data.frame(
    left = c(2, 4),
    right = c(4, 6),
    weight = c(0.4, 0.6),
    survival = c(1, 0.4)
  )
  times <- c(1, 3, 6, 7)  # 1 < min(left), e 7 > max(right)

  result <- single.TNBsurvival(data, times)
  # To time=1, expect 1
  expect_equal(result[1], 1)
  # To time=7, expect equal to survival in last point
  expect_equal(result[4], data$survival[nrow(data)])
})

test_that("single.TNBsurvival - handling NA in right", {
  data <- data.frame(
    left = c(2, 4, 6),
    right = c(4, NA, Inf),
    weight = c(0.3, 0.5, 0.2),
    survival = c(1.0, 0.7, 0.5)
  )
  times <- c(1, 3, 5, 7)

  result <- single.TNBsurvival(data, times)
  expect_type(result, "double")
  expect_equal(length(result), length(times))
})


test_that("single.TNBsurvival - linear interpolation", {
  data <- data.frame(
    left = c(2, 5),
    right = c(5, 8),
    weight = c(0.4, 0.6),
    survival = c(1.0, 0.4)
  )
  # Survival drops from 1.0 to 0.4 in the interval [5,8].
  # In the middle, say 6.5, it should be ~0.7 (interpolation).
  times <- c(3, 6, 6.5, 7)
  result <- single.TNBsurvival(data, times)

  # time=3 is in the interval [2,5] -> no change, should be survival of the previous point
  expect_equal(result[1], 1.0)

  # time=6.5 should be ~ interpolated between 1.0 and 0.4
  # Range: [5, 8] -> delta time = 3, delta surv = 0.4 - 1 = -0.6
  # elapsed time 6.5 - 5 = 1.5
  # interpolated = 1.0 + (-0.6)*(1.5/3) = 1.0 - 0.3 = 0.7
  expect_equal(result[3], 0.7, tolerance = 1e-6)
})


