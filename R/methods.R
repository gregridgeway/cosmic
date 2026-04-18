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
#' \code{summary.cosmic_fit()} returns the summary matrix produced by the
#' underlying \code{stanfit} summary method.
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
  .stan_summary_matrix(object$fit, ...)
}

.stan_summary_matrix <- function(fit, ...) {
  summary_candidates <- list(
    fit,
    tryCatch(rstan::summary(fit, ...), error = function(e) NULL),
    tryCatch(summary(fit, ...), error = function(e) NULL)
  )

  for (fit_summary in summary_candidates) {
    summary_matrix <- .extract_summary_matrix(fit_summary)
    if (!is.null(summary_matrix)) {
      return(summary_matrix)
    }
  }

  stop("Could not extract a summary matrix from the stanfit object.", call. = FALSE)
}

.extract_summary_matrix <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.matrix(x)) {
    return(.normalize_summary_matrix(x))
  }

  if (is.list(x) && !is.null(x$summary) && is.matrix(x$summary)) {
    return(.normalize_summary_matrix(x$summary))
  }

  if (is.list(x)) {
    matrix_elements <- vapply(x, is.matrix, logical(1))
    if (any(matrix_elements)) {
      return(.normalize_summary_matrix(x[[which(matrix_elements)[1]]]))
    }
  }

  NULL
}

.normalize_summary_matrix <- function(x) {
  attributes_to_keep <- attributes(x)
  attributes_to_keep$class <- NULL

  out <- unclass(x)
  attributes(out) <- attributes_to_keep
  out
}
