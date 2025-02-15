test_that("plot.TB generates a valid plot", {
  left <- c(2, 4, 6, 8)
  right <- c(4, 6, 8, NA)
  intervals <- survivalTB::TNBintervals(left, right, nboot = 10)

  expect_no_error(plot.TB(intervals, conf = TRUE, conf.level = 0.95))
})

test_that("plot.TBL generates a valid interactive plot", {
  left <- c(2, 4, 6, 8)
  right <- c(4, 6, 8, NA)
  intervals <- TNBsurvival::TNBintervals(left, right, nboot = 10)

  # Verifica se não há erros ao gerar o plot
  expect_no_error(plot.TBL(intervals, conf = TRUE, conf.level = 0.95))
})
