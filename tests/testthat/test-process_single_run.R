test_that(".process_single_run works correctly", {
  left <- c(2, 4, 6)
  right <- c(4, 6, NA)

  result <- survivalTB:::.process_single_run(left, right)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("left", "right", "weight", "survival"))
  expect_equal(nrow(result), 3)
})

test_that(".process_single_run - handling NA in right", {
  left <- c(1, 2, 3)
  right <- c(2, NA, 5)
  expect_no_error({
    result <- survivalTB:::.process_single_run(left, right)
    expect_s3_class(result, "data.frame")
  })
})

test_that(".process_single_run - handling Inf", {
  left <- c(2, 4, 6)
  right <- c(4, 6, Inf)
  result <- survivalTB:::.process_single_run(left, right)

  # Verifica se 'Inf' aparece corretamente na saída
  expect_true(any(is.infinite(result$right)))
})


test_that(".process_single_run - length mismatch", {
  left <- c(1, 2, 3)
  right <- c(2, 4)
  expect_error(
    survivalTB:::.process_single_run(left, right),
    regexp = "must have the same length"
  )
})





