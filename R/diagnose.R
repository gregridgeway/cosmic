#' Diagnose a COSMIC fit
#'
#' Reports convergence and Hamiltonian Monte Carlo diagnostics for a fitted
#' \code{"cosmic_fit"} object. The report flags parameters with large
#' \eqn{\hat{R}}, small effective sample size, and sampler pathologies such as
#' divergences, saturated tree depth, or low E-BFMI.
#'
#' @param object A fitted object of class \code{"cosmic_fit"}.
#' @param ... Additional arguments passed to the underlying summary method
#'   for the Stan fit.
#'
#' @return
#' \code{diagnose.cosmic_fit()} prints a compact diagnostic report and returns
#' a \code{"cosmic_diagnostics"} object invisibly.
#'
#' @examples
#' \dontrun{
#' fit <- cosmic(d, id, idOff, y, iter = 1000, chains = 4)
#' diagnose(fit)
#' }
#'
#' @name diagnose
#' @rdname diagnose
#' @export
diagnose <- function(object, ...) {
  UseMethod("diagnose")
}

#' @rdname diagnose
#' @param rhat_threshold Threshold above which \eqn{\hat{R}} is flagged.
#'   Default is \code{1.01}.
#' @param ess_threshold_per_chain Minimum effective sample size per chain.
#'   Default is \code{100}; the total threshold used is this value multiplied
#'   by the number of chains.
#' @param ebfmi_threshold Threshold below which E-BFMI is flagged.
#'   Default is \code{0.2}.
#' @export
diagnose.cosmic_fit <- function(object,
                                rhat_threshold = 1.01,
                                ess_threshold_per_chain = 100,
                                ebfmi_threshold = 0.2,
                                ...) {
  summary_matrix <- .stan_summary_matrix(object$fit, ...)
  sampler_diagnostics <- .collect_sampler_diagnostics(
    object$fit,
    ebfmi_threshold = ebfmi_threshold
  )

  out <- .build_cosmic_diagnostics(
    summary_matrix = summary_matrix,
    n_chains = sampler_diagnostics$n_chains,
    sampler_diagnostics = sampler_diagnostics,
    rhat_threshold = rhat_threshold,
    ess_threshold_per_chain = ess_threshold_per_chain,
    ebfmi_threshold = ebfmi_threshold
  )

  print(out)
  invisible(out)
}

#' @export
print.cosmic_diagnostics <- function(x, ...) {
  status <- if (isTRUE(x$ok)) "OK" else "Warnings detected"

  cat("COSMIC fit diagnostics:", status, "\n")
  cat(
    "  Max R-hat:",
    .format_numeric(x$metrics$max_rhat),
    "\n"
  )
  cat(
    "  Min n_eff:",
    .format_numeric(x$metrics$min_n_eff),
    "\n"
  )
  cat(
    "  Divergences:",
    x$metrics$divergences,
    "\n"
  )
  cat(
    "  Max treedepth saturations:",
    x$metrics$treedepth_saturated,
    "\n"
  )
  cat(
    "  Low E-BFMI chains:",
    if (length(x$metrics$low_ebfmi_chains)) {
      paste(x$metrics$low_ebfmi_chains, collapse = ", ")
    } else {
      "none"
    },
    "\n"
  )

  if (length(x$warnings)) {
    cat("Warnings:\n")
    for (warning in x$warnings) {
      cat(" - ", warning, "\n", sep = "")
    }
  } else {
    cat("No major diagnostic problems detected.\n")
  }

  invisible(x)
}

.build_cosmic_diagnostics <- function(summary_matrix,
                                      n_chains,
                                      sampler_diagnostics,
                                      rhat_threshold,
                                      ess_threshold_per_chain,
                                      ebfmi_threshold) {
  if (is.null(summary_matrix) || !is.matrix(summary_matrix)) {
    stop("summary_matrix must be a matrix.", call. = FALSE)
  }

  n_chains <- .coalesce_scalar_int(n_chains, 1L)
  ess_threshold <- ess_threshold_per_chain * n_chains

  rhat <- .summary_column(summary_matrix, "Rhat")
  n_eff <- .summary_column(summary_matrix, "n_eff")

  bad_rhat <- which(is.finite(rhat) & rhat > rhat_threshold)
  low_n_eff <- which(is.finite(n_eff) & n_eff < ess_threshold)

  metrics <- list(
    max_rhat = .safe_max(rhat),
    min_n_eff = .safe_min(n_eff),
    n_bad_rhat = length(bad_rhat),
    n_low_n_eff = length(low_n_eff),
    divergences = .coalesce_scalar_int(sampler_diagnostics$divergences, 0L),
    treedepth_saturated = .coalesce_scalar_int(
      sampler_diagnostics$treedepth_saturated,
      0L
    ),
    low_ebfmi_chains = sampler_diagnostics$low_ebfmi_chains %||% integer(),
    ebfmi = sampler_diagnostics$ebfmi %||% numeric()
  )

  warnings <- character()

  if (metrics$n_bad_rhat > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "%d parameter%s with R-hat > %.2f (max %.3f).",
        metrics$n_bad_rhat,
        if (metrics$n_bad_rhat == 1L) "" else "s",
        rhat_threshold,
        metrics$max_rhat
      )
    )
  }

  if (metrics$n_low_n_eff > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "%d parameter%s with n_eff < %d (%d per chain across %d chain%s; min %.0f).",
        metrics$n_low_n_eff,
        if (metrics$n_low_n_eff == 1L) "" else "s",
        ess_threshold,
        ess_threshold_per_chain,
        n_chains,
        if (n_chains == 1L) "" else "s",
        metrics$min_n_eff
      )
    )
  }

  if (metrics$divergences > 0L) {
    warnings <- c(
      warnings,
      sprintf("%d divergent transition%s detected.",
              metrics$divergences,
              if (metrics$divergences == 1L) "" else "s")
    )
  }

  if (metrics$treedepth_saturated > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "%d transition%s hit the maximum treedepth (%d).",
        metrics$treedepth_saturated,
        if (metrics$treedepth_saturated == 1L) "" else "s",
        .coalesce_scalar_int(sampler_diagnostics$max_treedepth, 10L)
      )
    )
  }

  if (length(metrics$low_ebfmi_chains) > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "Low E-BFMI detected in chain%s %s (threshold %.2f).",
        if (length(metrics$low_ebfmi_chains) == 1L) "" else "s",
        paste(metrics$low_ebfmi_chains, collapse = ", "),
        ebfmi_threshold
      )
    )
  }

  structure(
    list(
      ok = length(warnings) == 0L,
      warnings = warnings,
      metrics = metrics,
      thresholds = list(
        rhat = rhat_threshold,
        n_eff = ess_threshold,
        n_eff_per_chain = ess_threshold_per_chain,
        ebfmi = ebfmi_threshold
      ),
      parameters = list(
        high_rhat = rownames(summary_matrix)[bad_rhat],
        low_n_eff = rownames(summary_matrix)[low_n_eff]
      )
    ),
    class = "cosmic_diagnostics"
  )
}

.collect_sampler_diagnostics <- function(fit, ebfmi_threshold = 0.2) {
  sampler_params <- tryCatch(
    rstan::get_sampler_params(fit, inc_warmup = FALSE),
    error = function(e) NULL
  )

  n_chains <- length(sampler_params)

  divergences <- 0L
  treedepth_saturated <- 0L
  max_treedepth <- .get_max_treedepth(fit)
  ebfmi <- numeric()

  if (length(sampler_params) > 0L) {
    divergences <- sum(vapply(
      sampler_params,
      function(chain_params) {
        if ("divergent__" %in% colnames(chain_params)) {
          sum(chain_params[, "divergent__"], na.rm = TRUE)
        } else {
          0
        }
      },
      numeric(1)
    ))

    treedepth_saturated <- sum(vapply(
      sampler_params,
      function(chain_params) {
        if ("treedepth__" %in% colnames(chain_params)) {
          sum(chain_params[, "treedepth__"] >= max_treedepth, na.rm = TRUE)
        } else {
          0
        }
      },
      numeric(1)
    ))

    ebfmi <- vapply(
      sampler_params,
      .compute_ebfmi,
      numeric(1)
    )
  }

  list(
    n_chains = if (n_chains > 0L) n_chains else 1L,
    divergences = as.integer(divergences),
    treedepth_saturated = as.integer(treedepth_saturated),
    max_treedepth = max_treedepth,
    ebfmi = ebfmi,
    low_ebfmi_chains = which(is.finite(ebfmi) & ebfmi < ebfmi_threshold)
  )
}

.compute_ebfmi <- function(chain_params) {
  if (!"energy__" %in% colnames(chain_params)) {
    return(NA_real_)
  }

  energy <- as.numeric(chain_params[, "energy__"])
  energy <- energy[is.finite(energy)]

  if (length(energy) < 2L) {
    return(NA_real_)
  }

  numerator <- mean(diff(energy)^2)
  denominator <- stats::var(energy)

  if (!is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }

  numerator / denominator
}

.get_max_treedepth <- function(fit) {
  stan_args <- tryCatch(fit@stan_args, error = function(e) NULL)

  if (!length(stan_args)) {
    return(10L)
  }

  first_control <- stan_args[[1]]$control

  if (is.null(first_control) || is.null(first_control$max_treedepth)) {
    return(10L)
  }

  as.integer(first_control$max_treedepth)
}

.summary_column <- function(summary_matrix, name) {
  if (!name %in% colnames(summary_matrix)) {
    return(rep(NA_real_, nrow(summary_matrix)))
  }

  as.numeric(summary_matrix[, name])
}

.safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  max(x)
}

.safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  min(x)
}

.coalesce_scalar_int <- function(x, default) {
  if (length(x) == 0L || is.null(x) || !is.finite(x[1])) {
    return(as.integer(default))
  }

  as.integer(x[1])
}

.format_numeric <- function(x, digits = 3) {
  if (!length(x) || !is.finite(x)) {
    return("NA")
  }

  formatC(x, format = "f", digits = digits)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
