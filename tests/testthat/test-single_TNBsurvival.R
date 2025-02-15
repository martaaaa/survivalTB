test_that(".single.TNBsurvival works correctly", {
  data <- data.frame(
    left = c(2, 4, 6),
    right = c(4, 6, NA),
    weight = c(0.3, 0.5, 0.2),
    survival = c(1.0, 0.7, 0.2)
  )
  times <- c(1, 3, 5, 7)

  # Observe que usamos survivalTB::: ao invés de .single.TNBsurvival diretamente
  result <- survivalTB:::.single.TNBsurvival(data, times)
  expect_type(result, "double")
  expect_length(result, length(times))
})

test_that(".single.TNBsurvival - times out of range", {
  data <- data.frame(
    left = c(2, 4),
    right = c(4, 6),
    weight = c(0.4, 0.6),
    survival = c(1, 0.4)
  )
  times <- c(1, 3, 6, 7)  # 1 < min(left), e 7 > max(right)

  # Idem aqui, chamamos via survivalTB:::.single.TNBsurvival
  result <- survivalTB:::.single.TNBsurvival(data, times)
  expect_equal(result[1], 1)
  expect_equal(result[4], data$survival[nrow(data)])
})

test_that(".single.TNBsurvival - handling NA in right", {
  data <- data.frame(
    left = c(2, 4, 6),
    right = c(4, NA, Inf),
    weight = c(0.3, 0.5, 0.2),
    survival = c(1.0, 0.7, 0.5)
  )
  times <- c(1, 3, 5, 7)

  result <- survivalTB:::.single.TNBsurvival(data, times)
  expect_type(result, "double")
  expect_equal(length(result), length(times))
})

test_that(".single.TNBsurvival - linear interpolation", {
  data <- data.frame(
    left = c(2, 5),
    right = c(5, 8),
    weight = c(0.4, 0.6),
    survival = c(1.0, 0.4)
  )
  times <- c(3, 6, 6.5, 7)

  result <- survivalTB:::.single.TNBsurvival(data, times)

  expect_equal(result[1], 1.0)
  expect_equal(result[3], 0.7, tolerance = 1e-6)
})
