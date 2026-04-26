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
  expect_equal(out$officer_lookup,
               data.frame(idOff = c(1L, 2L), idOffOrig = c(1, 2)))
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
        officer_lookup = data.frame(
          idOff = c(1L, 2L, 3L),
          idOffOrig = c("A12", "B07", "C99")
        ),
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
  expect_equal(off_summary$idOffOrig, c("A12", "B07", "C99"))
  expect_equal(off_summary$idOff, c(1, 2, 3))
  expect_equal(off_summary$nPeers, c(2, 2, 2))
  expect_equal(off_summary$force1, c(1, 0, 1))
  expect_equal(off_summary$force2, c(0, 2, 0))
  expect_equal(off_summary$force3, c(1, 0, 1))
  expect_equal(off_summary$pRankToppct, c(1, 0, 0))
  expect_equal(off_summary$pRankBotpct, c(0, 0, 0))
  expect_equal(off_summary$lamMean, c(2, -0.25, -1.75))
})


test_that("outlier_report filters and orders an officer summary", {

  off_summary <- structure(data.frame(
    idOffOrig = c("A12", "B07", "C99"),
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
  ), class = c("cosmic_officer_summary", "data.frame"))

  report <- outlier_report(off_summary, prob_outlier = 0.8)

  expect_s3_class(report, "cosmic_outlier_report")
  expect_equal(report$idOffOrig, c("A12", "C99"))
  expect_equal(report$idOff, c(1, 3))
})


test_that("outlier_report returns all officers when prob_outlier is zero", {

  off_summary <- structure(data.frame(
    idOffOrig = c("A12", "B07", "C99"),
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
  ), class = c("cosmic_officer_summary", "data.frame"))

  report <- outlier_report(off_summary, prob_outlier = 0)

  expect_s3_class(report, "cosmic_outlier_report")
  expect_equal(report$idOffOrig, c("A12", "B07", "C99"))
  expect_equal(report$idOff, c(1, 2, 3))
  expect_equal(attr(report, "prob_outlier"), 0)
})


test_that("outlier_report rejects fitted models directly", {

  fit <- structure(list(), class = "cosmic_fit")

  expect_error(
    outlier_report(fit),
    "Call officer_summary\\(\\) first"
  )
})


test_that("diagnostics flag poor convergence and HMC issues", {

  summary_matrix <- cbind(
    n_eff = c(50, 410, 900),
    Rhat = c(1.005, 1.02, NA)
  )

  rownames(summary_matrix) <- c("lambda[1]", "lambda[2]", "lp__")

  diag <- .build_cosmic_diagnostics(
    summary_matrix = summary_matrix,
    n_chains = 4L,
    sampler_diagnostics = list(
      divergences = 2L,
      treedepth_saturated = 1L,
      max_treedepth = 10L,
      low_ebfmi_chains = 2L,
      ebfmi = c(0.35, 0.15)
    ),
    rhat_threshold = 1.01,
    ess_threshold_per_chain = 100,
    ebfmi_threshold = 0.2
  )

  expect_s3_class(diag, "cosmic_diagnostics")
  expect_false(diag$ok)
  expect_equal(diag$metrics$n_bad_rhat, 1)
  expect_equal(diag$metrics$n_low_n_eff, 1)
  expect_equal(diag$metrics$divergences, 2)
  expect_equal(diag$metrics$treedepth_saturated, 1)
  expect_equal(diag$metrics$low_ebfmi_chains, 2L)
  expect_match(diag$warnings[1], "R-hat > 1.01")
  expect_match(diag$warnings[2], "n_eff < 400")
  expect_match(diag$warnings[3], "divergent transition")
  expect_match(diag$warnings[4], "maximum treedepth")
  expect_match(diag$warnings[5], "Low E-BFMI")
  expect_equal(diag$parameters$high_rhat, "lambda[2]")
  expect_equal(diag$parameters$low_n_eff, "lambda[1]")
})


test_that("diagnostic printer reports clean fits", {

  summary_matrix <- cbind(
    n_eff = c(800, 900),
    Rhat = c(1.001, 1.003)
  )

  rownames(summary_matrix) <- c("lambda[1]", "lambda[2]")

  diag <- .build_cosmic_diagnostics(
    summary_matrix = summary_matrix,
    n_chains = 4L,
    sampler_diagnostics = list(
      divergences = 0L,
      treedepth_saturated = 0L,
      max_treedepth = 10L,
      low_ebfmi_chains = integer(),
      ebfmi = c(0.5, 0.7)
    ),
    rhat_threshold = 1.01,
    ess_threshold_per_chain = 100,
    ebfmi_threshold = 0.2
  )

  output <- capture.output(print(diag))

  expect_true(diag$ok)
  expect_match(output[1], "COSMIC fit diagnostics: OK")
  expect_match(output[length(output)], "No major diagnostic problems detected")
})


test_that("stan summary extractor handles matrix and list outputs", {

  summary_matrix <- cbind(
    n_eff = c(100, 200),
    Rhat = c(1, 1.01)
  )

  summary.fake_fit_summary <- function(object, ...) object
  summary.fake_fit_summary_list <- function(object, ...) object

  expect_equal(
    .stan_summary_matrix(structure(summary_matrix, class = "fake_fit_summary")),
    summary_matrix
  )

  expect_equal(
    .stan_summary_matrix(structure(list(summary = summary_matrix), class = "fake_fit_summary_list")),
    summary_matrix
  )
})


.complete_enumeration_logcl <- function(d, lambda, s) {
  incident_rows <- split(seq_len(nrow(d)), d$id)
  log_numerator <- sum(lambda[d$idOff] * s[d$y])

  incident_denominators <- vapply(incident_rows, function(rows) {
    counts <- table(factor(d$y[rows], levels = seq_along(s)))
    assignments <- arrangements::permutations(length(s), freq = counts)

    sum(exp(apply(assignments, 1, function(y_perm) {
      sum(lambda[d$idOff[rows]] * s[y_perm])
    })))
  }, numeric(1))

  log_numerator - sum(log(incident_denominators))
}

.compiled_stan_logcl <- function(model, d, lambda, s) {
  n_force_types <- length(s)
  prior_sd_lambda <- 2
  prior_sd_sdiff <- 1
  tmp <- tempfile("cosmic-log-prob")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  data_file <- file.path(tmp, "data.json")
  params_file <- file.path(tmp, "params.json")
  output_file <- file.path(tmp, "lp.csv")

  cmdstanr::write_stan_json(
    list(
      y = as.integer(d$y),
      id = as.integer(d$id),
      idOff = as.integer(d$idOff),
      startIndex = as.integer(which(!duplicated(d$id))),
      nMaxOffs = as.integer(max(table(d$id))),
      nRows = as.integer(nrow(d)),
      nOff = as.integer(max(d$idOff)),
      nIncidents = as.integer(length(unique(d$id))),
      nForceTypes = as.integer(n_force_types),
      rPriorSD_lambda = prior_sd_lambda,
      rPriorSD_sDiff = prior_sd_sdiff
    ),
    data_file
  )
  cmdstanr::write_stan_json(
    list(
      lambda = as.numeric(lambda),
      sDelta = array(as.numeric(diff(s)[-1]), dim = n_force_types - 2L)
    ),
    params_file
  )

  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  Sys.setenv(PATH = paste(
    c(file.path(cmdstanr::cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb"),
      old_path),
    collapse = .Platform$path.sep
  ))

  result <- system2(
    model$exe_file(),
    c(
      "data", paste0("file=", data_file),
      "num_threads=1",
      "method=log_prob",
      paste0("constrained_params=", params_file),
      "jacobian=0",
      "output", paste0("file=", output_file)
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!is.null(attr(result, "status"))) {
    fail(paste(result, collapse = "\n"))
  }

  lp <- read.csv(output_file, comment.char = "#", check.names = FALSE)[["lp__"]]
  prior_kernel <- -0.5 * sum((lambda / prior_sd_lambda)^2) -
    0.5 * sum((diff(s)[-1] / prior_sd_sdiff)^2)

  lp - prior_kernel
}

test_that("DP matches complete enumeration across incident shapes", {

  stan_file <- system.file("stan", "cosmic.stan", package = "cosmic")
  if (!nzchar(stan_file)) {
    skip("Bundled Stan source not found")
  }

  model <- cmdstanr::cmdstan_model(
    stan_file,
    cpp_options = list(stan_threads = TRUE),
    quiet = TRUE
  )

  cases <- list(
    list(
      name = "three force types with two-officer incidents",
      s = c(0, 1, 1.6),
      lambda = c(-0.8, -0.1, 0.5, 1.2),
      d = data.frame(
        id = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5),
        idOff = c(1, 2, 2, 3, 1, 3, 4, 2, 4, 1, 2, 4),
        y = c(1, 3, 2, 1, 1, 2, 3, 2, 2, 3, 1, 2)
      )
    ),
    list(
      name = "four force types with repeated outcomes",
      s = c(0, 1, 1.5, 2.2),
      lambda = seq(-1, 1, length.out = 6),
      d = data.frame(
        id = c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 6, 6),
        idOff = c(1, 2, 1, 2, 3, 1, 2, 3, 2, 3, 1, 3, 1, 2, 3, 4, 5, 6),
        y = c(1, 2, 3, 2, 1, 4, 1, 1, 2, 1, 3, 4, 1, 2, 3, 4, 1, 2)
      )
    ),
    list(
      name = "five force types with wider y range",
      s = c(0, 1, 1.4, 2.1, 3.0),
      lambda = c(-1.3, -0.4, 0.2, 0.9, 1.4),
      d = data.frame(
        id = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4),
        idOff = c(1, 2, 1, 2, 3, 4, 2, 4, 5, 1, 2, 3, 4, 5),
        y = c(1, 5, 2, 3, 4, 5, 1, 3, 5, 2, 2, 4, 5, 1)
      )
    )
  )

  for (case in cases) {
    expect_equal(
      .compiled_stan_logcl(model, case$d, case$lambda, case$s),
      .complete_enumeration_logcl(case$d, case$lambda, case$s),
      tolerance = 1e-8,
      info = case$name
    )
  }
})


