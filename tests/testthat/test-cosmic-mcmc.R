test_that("cosmic_mcmc returns cosmic_fit", {
  set.seed(1)
  n <- 60
  x <- stats::rnorm(n)
  z <- 0.8 * x + stats::rnorm(n)
  y <- ifelse(z > 0, 2L, 1L)

  fit <- cosmic_mcmc(y ~ x, data = data.frame(y = y, x = x), iter = 200, burnin = 50)

  expect_s3_class(fit, "cosmic_fit")
  expect_true(is.matrix(fit$draws))
  expect_equal(nrow(fit$draws), 150)
  expect_true(fit$acceptance_rate > 0)
})

test_that("summary method works", {
  set.seed(2)
  n <- 40
  x <- stats::rnorm(n)
  y <- ifelse(x + stats::rnorm(n) > 0, 2L, 1L)

  fit <- cosmic_mcmc(y ~ x, data = data.frame(y = y, x = x), iter = 150, burnin = 50)
  s <- summary(fit)

  expect_s3_class(s, "summary.cosmic_fit")
  expect_true(all(c("mean", "sd", "q05", "q50", "q95") %in% colnames(s$coefficients)))
})
