#' Fit the COSMIC model via MCMC
#'
#' Fits a COSMIC model using a Markov chain Monte Carlo routine.
#' This function wraps `mcmcOrdinalStereotype()` and returns an object
#' of class `cosmic_fit`.
#'
#' @param formula A two-category outcome formula.
#' @param data Data frame containing model variables.
#' @param iter Total MCMC iterations.
#' @param burnin Number of burn-in iterations.
#' @param step_size Proposal standard deviation for random-walk MH.
#' @param prior_sd Prior standard deviation.
#'
#' @return An object of class `cosmic_fit`.
#' @export
cosmic_mcmc <- function(formula,
                        data,
                        iter = 2000,
                        burnin = floor(iter / 2),
                        step_size = 0.1,
                        prior_sd = 5) {
  mf <- stats::model.frame(formula, data = data)
  y <- stats::model.response(mf)

  if (is.factor(y)) {
    if (nlevels(y) != 2) {
      stop("Only two-class outcomes are currently supported.")
    }
    y <- as.integer(y)
  }

  if (!is.integer(y)) {
    y <- as.integer(y)
  }

  if (!all(y %in% c(1L, 2L))) {
    stop("Outcome must be encoded as 1/2 (or a factor with two levels).")
  }

  X <- stats::model.matrix(stats::delete.response(stats::terms(mf)), data = mf)

  fit <- mcmcOrdinalStereotype(
    X = X,
    y = y,
    iter = iter,
    burnin = burnin,
    step_size = step_size,
    prior_sd = prior_sd
  )

  structure(
    c(
      fit,
      list(
        formula = formula,
        call = match.call(),
        n = nrow(X),
        p = ncol(X)
      )
    ),
    class = "cosmic_fit"
  )
}
