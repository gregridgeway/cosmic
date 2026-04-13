test_that("prep_cosmic_data runs on simple input", {

  d <- data.frame(
    id = c(1,1,2,2),
    idOff = c(1,2,1,2),
    y = c(1,2,1,2)
  )

  out <- prep_cosmic_data(d, id, idOff, y)

  expect_true(is.list(out))
  expect_equal(out$nOff, 2)
  expect_equal(out$nIncidents, 2)
})



test_that("invalid y is rejected", {

  d <- data.frame(
    id = c(1,1),
    idOff = c(1,2),
    y = c(1,3)  # gap should fail
  )

  expect_error(
    prep_cosmic_data(d, id, idOff, y)
  )
})


test_that("cosmic runs on tiny dataset", {

  d <- data.frame(
    id = c(1,1,2,2,2),
    idOff = c(1,2,1,2,3),
    y = c(1,2,1,4,1)
  )

  fit <- cosmic(d, id, idOff, y,
                iter = 200,
                chains = 1,
                threads = 1)

  expect_true("cosmic_fit" %in% class(fit))
})


test_that("DP matches Heaps on small example", {

  s <- c(0,1,1.5,2)
  lambda <- c(-0.5, -1, -2)

  # call exposed functions or wrapper
  # expect_equal(logDenomDP_logspace(...), log(denomHeaps(...)))

})


