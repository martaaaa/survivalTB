test_that("plot.TB (static) generates a valid plot", {
  left <- c(2, 4, 6, 8)
  right <- c(4, 6, 8, NA)
  intervals <- survivalTB::TNBintervals(left, right, nboot = 10)

  # Chama plot() -> despacha para plot.TB() com interactive=FALSE por default
  expect_no_error(plot(intervals, conf = TRUE, conf.level = 0.95))
})

test_that("plot.TB (interactive) generates a valid plot", {
  left <- c(2, 4, 6, 8)
  right <- c(4, 6, 8, NA)
  intervals <- survivalTB::TNBintervals(left, right, nboot = 10)

  # Chama plot() -> despacha para plot.TB() com interactive=TRUE
  expect_no_error(plot(intervals, interactive = TRUE, conf = TRUE, conf.level = 0.95))
})
