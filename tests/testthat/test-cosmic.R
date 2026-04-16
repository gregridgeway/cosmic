
get_exposed_cosmic_stan_functions <- local({
  exposed_env <- NULL

  function() {
    if (!is.null(exposed_env)) {
      return(exposed_env)
    }

    skip_if_not_installed("rstan")

    stan_file <- file.path("inst", "stan", "cosmic.stan")
    if (!file.exists(stan_file)) {
      skip("Bundled Stan source not found")
    }

    stan_lines <- readLines(stan_file, warn = FALSE)
    start <- grep("^functions\\s*\\{", stan_lines)[1]
    if (is.na(start)) {
      skip("No functions block found in bundled Stan model")
    }

    depth <- 0L
    end <- NA_integer_
    for (i in seq.int(start, length(stan_lines))) {
      depth <- depth + lengths(regmatches(stan_lines[i], gregexpr("\\{", stan_lines[i])))
      depth <- depth - lengths(regmatches(stan_lines[i], gregexpr("\\}", stan_lines[i])))
      if (depth == 0L) {
        end <- i
        break
      }
    }

    if (is.na(end)) {
      skip("Could not isolate functions block from bundled Stan model")
    }

    stan_model <- rstan::stanc(
      model_code = paste(stan_lines[start:end], collapse = "\n"),
      model_name = "cosmic_functions"
    )

    exposed_env <<- new.env(parent = emptyenv())
    rstan::expose_stan_functions(stan_model, env = exposed_env)
    exposed_env
  }
})




test_that("prep_cosmic_data runs on simple input", {

  d <- data.frame(
    id = c(1,1,2,2),
    idOff = c(1,2,1,2),
    y = c(1,2,2,3)
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
    prep_cosmic_data(d, id, idOff, y),
    "consecutive")
})


test_that("cosmic runs on tiny dataset", {

  d <- data.frame(
    id = c(1,1,2,2,2),
    idOff = c(1,2,1,2,3),
    y = c(1,2,1,3,2)
  )

  fit <- cosmic(d, id, idOff, y,
                iter = 200,
                chains = 1,
                threads = 1)

  expect_true("cosmic_fit" %in% class(fit))
})


test_that("fails with fewer than 3 force types", {

  d <- data.frame(
    id    = c(1,1,2,2),
    idOff = c(1,2,1,2),
    y     = c(1,2,1,2)
  )

  expect_error(
    prep_cosmic_data(d, id, idOff, y),
    "at least 3"
  )
})


test_that("officer_summary works with supplied draws", {

  mock_fit <- structure(
    list(
      data = list(
        idOff = c(1, 2, 1, 2, 3, 3),
        y = c(1, 2, 3, 2, 1, 3),
        nOff = 3L,
        nForceTypes = 3L,
        rPriorSD_lambda = 2
      )
    ),
    class = "cosmic_fit"
  )

  lambda_draws <- cbind(
    rep(1.5, 20),
    rep(0.0, 20),
    rep(-1.0, 20)
  )

  off_summary <- .officer_summary_from_lambda_draws(
    object = mock_fit,
    lambda_draws = lambda_draws,
    pct_threshold = 0.25,
    pct_tail = 0.2
  )

  expect_s3_class(off_summary, "cosmic_officer_summary")
  expect_equal(nrow(off_summary), 3)
  expect_equal(off_summary$nPeers, c(2, 2, 2))
  expect_equal(off_summary$force1, c(1, 0, 1))
  expect_equal(off_summary$force2, c(0, 2, 0))
  expect_equal(off_summary$force3, c(1, 0, 1))
  expect_equal(off_summary$pRankToppct, c(1, 0, 0))
  expect_equal(off_summary$pRankBotpct, c(0, 0, 0))
  expect_equal(off_summary$lamMean, c(2, -0.25, -1.75))
})


test_that("outlier_report filters and orders an officer summary", {

  off_summary <- data.frame(
    idOff = c(1, 2, 3),
    nPeers = c(4, 4, 4),
    nInc = c(10, 9, 8),
    force1 = c(1, 2, 3),
    force2 = c(4, 5, 6),
    force3 = c(7, 8, 9),
    pRankToppct = c(0.95, 0.20, 0.10),
    pRankBotpct = c(0.01, 0.25, 0.91),
    lamMean = c(1.2, 0.1, -1.3),
    lam025 = c(0.9, -0.2, -1.8),
    lam975 = c(1.5, 0.4, -0.8)
  )

  report <- outlier_report(off_summary, prob_outlier = 0.8)

  expect_s3_class(report, "cosmic_outlier_report")
  expect_equal(report$idOff, c(1, 3))
})


test_that("DP matches complete enumeration across incident shapes", {
  skip_on_cran()
  skip_if_not_installed("arrangements")

  stan_file <- file.path("inst", "stan", "cosmic.stan")
  if (!file.exists(stan_file)) {
    skip("Bundled Stan source not found")
  }

  stan_lines <- readLines(stan_file, warn = FALSE)
  start <- grep("^functions\\s*\\{", stan_lines)[1]
  if(is.na(start)) {
    skip("No functions block found in bundled Stan model")
  }
  end <- grep("} // end functions", stan_lines)[1]
  if(is.na(end)) {
    skip("Could not isolate functions block from bundled Stan model")
  }

  rstan::expose_stan_functions(
    rstan::stanc(model_code = paste(stan_lines[start:end], collapse = "\n")))

  cases <- list(
    list( # incident 4 has y=(2,2)
      name = "three force types with two-officer incidents",
      s = c(0, 1, 1.6),
      lambda = c(-0.8, -0.1, 0.5, 1.2),
      d = data.frame(
        id = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5),
        idOff = c(1, 2, 2, 3, 1, 3, 4, 2, 4, 1, 2, 4),
        y = c(1, 3, 2, 1, 1, 2, 3, 2, 2, 3, 1, 2))),
    list(
      name = "four force types with repeated outcomes",
      s = c(0, 1, 1.5, 2.2),
      lambda = seq(-1, 1, length.out = 6),
      d = data.frame(id = c(1, 1,
                            2, 2, 2,
                            3, 3, 3,
                            4, 4,
                            5, 5,
                            6, 6, 6, 6, 6, 6),
                     idOff = c(1, 2,
                               1, 2, 3,
                               1, 2, 3,
                               2, 3,
                               1, 3,
                               1, 2, 3, 4, 5, 6),
                     y = c(1, 2,
                           3, 2, 1,
                           4, 1, 1,
                           2, 1,
                           3, 4,
                           1, 2, 3, 4, 1, 2))),
    list(
      name = "five force types with wider y range",
      s = c(0, 1, 1.4, 2.1, 3.0),
      lambda = c(-1.3, -0.4, 0.2, 0.9, 1.4),
      d = data.frame(id = c(1, 1,
                            2, 2, 2, 2,
                            3, 3, 3,
                            4, 4, 4, 4, 4),
                     idOff = c(1, 2,
                               1, 2, 3, 4,
                               2, 4, 5,
                               1, 2, 3, 4, 5),
                     y = c(1, 5,
                           2, 3, 4, 5,
                           1, 3, 5,
                           2, 2, 4, 5, 1))))

  for (case in cases) {
    d <- case$d
    lambda <- case$lambda
    s <- case$s

    nMaxOffs <- as.integer(max(table(d$id)))
    J <- as.integer(length(s))

    # compute logCL with direct enumeration
    incident_rows <- split(seq_len(nrow(d)), d$id)
    logLL_R <- sum(lambda[d$idOff] * s[d$y])

    incident_denoms <- vapply(incident_rows, function(rows) {
      counts <- table(factor(d$y[rows], levels = seq_along(s)))
      assignments <- arrangements::permutations(length(s), freq = counts)

      sum(exp(apply(assignments, 1, function(y_perm) {
        sum(lambda[d$idOff[rows]] * s[y_perm])
      })))
    }, numeric(1))

    expected <- logLL_R - sum(log(incident_denoms))

    # compute with Stan code
    actual <- logCL(
      ivY = as.integer(d$y),
      ivID = as.integer(d$id),
      ivOffID = as.integer(d$idOff),
      ivStartIndex = as.integer(which(!duplicated(d$id))),
      rvLambda = as.numeric(lambda),
      rvSDelta = as.numeric(diff(s)[-1]),
      nRows = as.integer(nrow(d)),
      nMaxOffs = nMaxOffs,
      nForceTypes = J,
      grainsize = 1L)

    expect_equal(
      actual,
      expected,
      tolerance = 1e-10,
      info = case$name)
  }
})
