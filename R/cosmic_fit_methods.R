#' @export
print.cosmic_fit <- function(x, ...) {
  cat("COSMIC fit\n")
  cat("Iterations:", x$iter, "(burn-in:", x$burnin, ")\n")
  cat("Observations:", x$n, " Parameters:", x$p, "\n")
  cat("Acceptance rate:", sprintf("%.3f", x$acceptance_rate), "\n")
  invisible(x)
}

#' Summarize a COSMIC fit
#'
#' @param object A `cosmic_fit` object.
#' @param ... Unused.
#'
#' @return A summary table with posterior means and quantiles.
#' @export
summary.cosmic_fit <- function(object, ...) {
  draws <- object$draws
  stats <- cbind(
    mean = colMeans(draws),
    sd = apply(draws, 2, stats::sd),
    q05 = apply(draws, 2, stats::quantile, probs = 0.05),
    q50 = apply(draws, 2, stats::quantile, probs = 0.50),
    q95 = apply(draws, 2, stats::quantile, probs = 0.95)
  )
  out <- list(
    coefficients = stats,
    acceptance_rate = object$acceptance_rate,
    iter = object$iter,
    burnin = object$burnin
  )
  class(out) <- "summary.cosmic_fit"
  out
}

#' @export
print.summary.cosmic_fit <- function(x, ...) {
  cat("COSMIC posterior summary\n")
  printCoefmat(x$coefficients)
  cat("Acceptance rate:", sprintf("%.3f", x$acceptance_rate), "\n")
  invisible(x)
}
