#' Internal MCMC engine for ordinal stereotype fitting
#'
#' This is the internal sampler used by [cosmic_mcmc()].
#'
#' @param X Numeric model matrix.
#' @param y Integer vector of class labels encoded as 1/2.
#' @param iter Total MCMC iterations.
#' @param burnin Number of burn-in draws.
#' @param step_size Proposal standard deviation for random-walk MH.
#' @param prior_sd Prior standard deviation.
#'
#' @returns A list with posterior draws and diagnostics.
#' @keywords internal
mcmcOrdinalStereotype <- function(X,
                                  y,
                                  iter = 2000,
                                  burnin = floor(iter / 2),
                                  step_size = 0.1,
                                  prior_sd = 5) {
  stopifnot(is.matrix(X), is.numeric(X))
  stopifnot(is.integer(y), all(y %in% c(1L, 2L)))
  stopifnot(iter > burnin, burnin >= 0)

  p <- ncol(X)
  beta <- rep(0, p)
  draws <- matrix(NA_real_, nrow = iter - burnin, ncol = p)
  colnames(draws) <- colnames(X)

  log_prior <- function(b) {
    sum(stats::dnorm(b, mean = 0, sd = prior_sd, log = TRUE))
  }

  current <- cpp_ordinal_loglik(X, y, beta, 2L) + log_prior(beta)
  accepted <- 0L
  keep <- 1L

  for (i in seq_len(iter)) {
    proposal <- beta + stats::rnorm(p, 0, step_size)
    candidate <- cpp_ordinal_loglik(X, y, proposal, 2L) + log_prior(proposal)
    if (log(stats::runif(1)) < (candidate - current)) {
      beta <- proposal
      current <- candidate
      accepted <- accepted + 1L
    }

    if (i > burnin) {
      draws[keep, ] <- beta
      keep <- keep + 1L
    }
  }

  list(
    draws = draws,
    acceptance_rate = accepted / iter,
    iter = iter,
    burnin = burnin,
    step_size = step_size,
    prior_sd = prior_sd
  )
}
