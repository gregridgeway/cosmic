#' Print and summarize a COSMIC fit
#'
#' Methods for inspecting fitted objects of class \code{"cosmic_fit"}.
#' \code{print()} shows a compact Stan summary for the main model parameters,
#' while \code{summary()} returns the full summary matrix from the underlying
#' \code{stanfit} object.
#'
#' @param x A fitted object of class \code{"cosmic_fit"}.
#' @param object A fitted object of class \code{"cosmic_fit"}.
#' @param ... Additional arguments passed to the underlying method.
#'
#' @return
#' \code{print.cosmic_fit()} returns \code{x} invisibly.
#'
#' \code{summary.cosmic_fit()} returns the summary matrix produced by
#' \code{summary(object$fit)$summary}.
#'
#' @name summary.cosmic_fit
#' @rdname summary.cosmic_fit
#' @export
print.cosmic_fit <- function(x, ...) {
  print(x$fit, pars = c("lambda", "sDelta"))
}

#' @rdname summary.cosmic_fit
#' @export
summary.cosmic_fit <- function(object, ...) {
  summary(object$fit)$summary
}
