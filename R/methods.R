#' Print and summarize a COSMIC fit
#'
#' Methods for inspecting fitted objects of class \code{"cosmic_fit"}.
#' \code{print()} shows a compact posterior summary for the main model parameters,
#' while \code{summary()} returns the full summary matrix from the underlying
#' CmdStan fit object.
#'
#' @param x A fitted object of class \code{"cosmic_fit"}.
#' @param object A fitted object of class \code{"cosmic_fit"}.
#' @param ... Additional arguments passed to the underlying method.
#'
#' @return
#' \code{print.cosmic_fit()} returns \code{x} invisibly.
#'
#' \code{summary.cosmic_fit()} returns the summary matrix produced by the
#' underlying fit summary method.
#'
#' @name summary.cosmic_fit
#' @rdname summary.cosmic_fit
#' @export
print.cosmic_fit <- function(x, ...) {
  print(x$fit$summary(variables = c("lambda", "sDelta"), ...))
  invisible(x)
}

#' @rdname summary.cosmic_fit
#' @export
summary.cosmic_fit <- function(object, ...) {
  .stan_summary_matrix(object$fit, ...)
}

.stan_summary_matrix <- function(fit, ...) {
  direct_summary <- .extract_summary_matrix(fit)
  if (!is.null(direct_summary)) {
    return(direct_summary)
  }

  summary_candidates <- list(
    tryCatch(fit$summary(...), error = function(e) NULL),
    tryCatch(summary(fit, ...), error = function(e) NULL)
  )

  for (fit_summary in summary_candidates) {
    summary_matrix <- .extract_summary_matrix(fit_summary)
    if (!is.null(summary_matrix)) {
      return(summary_matrix)
    }
  }

  stop("Could not extract a summary matrix from the fitted Stan object.", call. = FALSE)
}

.extract_summary_matrix <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.matrix(x)) {
    return(.normalize_summary_matrix(x))
  }

  if (is.data.frame(x) && "variable" %in% names(x)) {
    return(.normalize_cmdstan_summary(x))
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

.normalize_cmdstan_summary <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  rownames(out) <- out$variable
  out$variable <- NULL

  rename_map <- c(
    mean = "mean",
    median = "median",
    sd = "sd",
    mad = "mad",
    q5 = "5%",
    q95 = "95%",
    rhat = "Rhat",
    ess_bulk = "n_eff"
  )

  shared_names <- intersect(names(rename_map), names(out))
  names(out)[match(shared_names, names(out))] <- rename_map[shared_names]

  .normalize_summary_matrix(as.matrix(out))
}

.normalize_summary_matrix <- function(x) {
  attributes_to_keep <- attributes(x)
  attributes_to_keep$class <- NULL

  out <- unclass(x)
  attributes(out) <- attributes_to_keep
  out
}
