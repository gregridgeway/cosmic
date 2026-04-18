#' Summarize officers relative to their peer groups
#'
#' Computes posterior summary measures for each officer in a fitted COSMIC
#' model. The returned data frame includes the size of each officer's peer
#' group, tail-rank probabilities within that group, posterior contrasts
#' against the mean of peers, and counts by force category.
#'
#' Officers are treated as peers when the posterior variance of
#' \eqn{\lambda_i - \lambda_j} is smaller than
#' \code{pct_threshold * 2 * priorSD_lambda^2}.
#'
#' When the \pkg{future.apply} package is installed, computations are
#' distributed using the active \pkg{future} plan. If \pkg{progressr} is also
#' installed, progress updates are emitted while officers are processed.
#'
#' Parallel workers are not configured by \code{officer_summary()} itself.
#' Instead, the function respects the \pkg{future} plan that is already active.
#' For example, users can request four background R sessions with
#' \code{future::plan(future::multisession, workers = 4)} before calling
#' \code{officer_summary()}. If no multi-worker plan is active, computation
#' falls back to sequential evaluation. To see progress updates in an
#' interactive session, call \code{progressr::handlers("cli")} before running
#' \code{officer_summary()}.
#'
#' @param object A fitted object of class \code{"cosmic_fit"}.
#' @param pct_threshold Threshold used to define peer groups from posterior
#'   variances. Officer \code{j} is treated as a peer of officer \code{i} when
#'   the posterior variance of \code{lambda_i - lambda_j} is less than
#'   \code{pct_threshold * 2 * priorSD_lambda^2}. Default is \code{0.25}.
#' @param pct_tail Tail fraction used when computing top-tail and bottom-tail
#'   rank probabilities. Default is \code{0.05}, tracking the probability of
#'   being in the top or bottom 5 percent.
#'
#' @return A data frame with one row per officer. The returned object has class
#'   \code{c("cosmic_officer_summary", "data.frame")}.
#'
#' @examples
#' \dontrun{
#' future::plan(future::multisession, workers = 4)
#' progressr::handlers("cli")
#'
#' fit <- cosmic(d, id, idOff, y)
#' off_summary <- officer_summary(fit)
#' outlier_report(off_summary)
#' }
#'
#' @export
officer_summary <- function(object,
                            pct_threshold = 0.25,
                            pct_tail = 0.05) {
  if (!inherits(object, "cosmic_fit")) {
    stop("object must inherit from 'cosmic_fit'", call. = FALSE)
  }

  lambda_draws <- posterior(object, pars = "lambda")$lambda

  .officer_summary_from_lambda_draws(
    object = object,
    lambda_draws = lambda_draws,
    pct_threshold = pct_threshold,
    pct_tail = pct_tail
  )
}


.officer_summary_from_lambda_draws <- function(object,
                                               lambda_draws,
                                               pct_threshold = 0.25,
                                               pct_tail = 0.05) {
  if (!inherits(object, "cosmic_fit")) {
    stop("object must inherit from 'cosmic_fit'", call. = FALSE)
  }

  if (!is.numeric(pct_threshold) || length(pct_threshold) != 1L ||
      is.na(pct_threshold) || pct_threshold <= 0 || pct_threshold > 1) {
    stop("pct_threshold must be a positive numeric scalar", call. = FALSE)
  }

  if (!is.numeric(pct_tail) || length(pct_tail) != 1L ||
      is.na(pct_tail) || pct_tail <= 0 || pct_tail >= 0.5) {
    stop("pct_tail must be a numeric scalar between 0 and 0.5", call. = FALSE)
  }

  if (is.null(object$data$rPriorSD_lambda)) {
    stop("object$data$rPriorSD_lambda is required", call. = FALSE)
  }

  if (is.list(lambda_draws)) {
    lambda_draws <- lambda_draws$lambda
  }

  if (!is.matrix(lambda_draws)) {
    stop("draws must be a matrix or a posterior draw list containing lambda",
         call. = FALSE)
  }

  n_off <- object$data$nOff
  if (ncol(lambda_draws) != n_off) {
    stop("lambda draws must have one column per officer", call. = FALSE)
  }

  n_force_types <- object$data$nForceTypes
  force_names <- paste0("force", seq_len(n_force_types))
  var_threshold <- pct_threshold * 2 * object$data$rPriorSD_lambda^2
  id_off <- object$data$idOff
  y <- object$data$y

  n_inc <- tabulate(id_off, nbins = n_off)
  force_count_matrix <- vapply(
    seq_len(n_force_types),
    function(force_idx) {
      tabulate(id_off[y == force_idx], nbins = n_off)
    },
    integer(n_off)
  )
  colnames(force_count_matrix) <- force_names

  Sigma <- if (n_off == 1L) {
    matrix(0, nrow = 1L, ncol = 1L)
  } else {
    stats::var(lambda_draws)
  }
  Sigma_diff <- outer(diag(Sigma), diag(Sigma), "+") - 2 * Sigma

  worker_context <- list(
    Sigma_diff = Sigma_diff,
    var_threshold = var_threshold,
    force_count_matrix = force_count_matrix,
    force_names = force_names,
    lambda_draws = lambda_draws,
    pct_tail = pct_tail,
    n_inc = n_inc
  )

  rows <- .officer_summary_apply(seq_len(n_off), worker_context)

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  class(out) <- c("cosmic_officer_summary", "data.frame")
  attr(out, "pct_threshold") <- pct_threshold
  attr(out, "pct_tail") <- pct_tail
  attr(out, "var_threshold") <- var_threshold

  return(out)
}

.officer_summary_rank <- function(x)
{
  1L + sum(x[1] > x[-1])
}

.officer_summary_one <- function(i_id, context, p = NULL)
{
  if (!is.null(p)) {
    p(sprintf("Processing officer %d", i_id))
  }

  i_peers <- setdiff(which(context$Sigma_diff[i_id, ] < context$var_threshold), i_id)
  n_peers <- length(i_peers)

  force_counts <- stats::setNames(
    as.list(context$force_count_matrix[i_id, ]),
    context$force_names
  )

  if (n_peers == 0L) {
    stats <- list(
      pRankToppct = NA_real_,
      pRankBotpct = NA_real_,
      lamMean = NA_real_,
      lam025 = NA_real_,
      lam975 = NA_real_
    )
  } else {
    ranks <- apply(
      context$lambda_draws[, c(i_id, i_peers), drop = FALSE],
      1,
      .officer_summary_rank
    )
    lam_contrast <- context$lambda_draws[, i_id] -
      rowMeans(context$lambda_draws[, i_peers, drop = FALSE])

    stats <- list(
      pRankToppct = mean(ranks >= (1 - context$pct_tail) * (n_peers + 1)),
      pRankBotpct = mean(ranks <= context$pct_tail * (n_peers + 1)),
      lamMean = mean(lam_contrast),
      lam025 = as.numeric(stats::quantile(lam_contrast, probs = 0.025)),
      lam975 = as.numeric(stats::quantile(lam_contrast, probs = 0.975))
    )
  }

  return(as.data.frame(c(list(idOff = i_id,
                              nPeers = n_peers,
                              nInc = context$n_inc[i_id]),
                         force_counts,
                         stats), check.names = FALSE))
}

.officer_summary_apply <- function(indices, context)
{
  use_progress <- requireNamespace("progressr", quietly = TRUE)
  use_future <- requireNamespace("future.apply", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE) &&
    future::nbrOfWorkers() > 1L

  apply_indices <- function(p = NULL)
  {
    if (use_future) {
      return(future.apply::future_lapply(
        indices,
        .officer_summary_one,
        context = context,
        p = p,
        future.globals = list(
          context = context,
          .officer_summary_one = .officer_summary_one,
          .officer_summary_rank = .officer_summary_rank
        ),
        future.packages = "stats"
      ))
    }

    lapply(indices, .officer_summary_one, context = context, p = p)
  }

  if (use_progress)
  {
    return(progressr::with_progress({
      p <- progressr::progressor(along = indices)
      apply_indices(p = p)
    }))
  } else
  {
    return(apply_indices())
  }
}


#' Create an outlier-focused report table
#'
#' Filters and reorders officer summaries to show only officers whose
#' top-tail or bottom-tail posterior rank probability exceeds a chosen cutoff.
#' The returned data frame is designed to be passed directly to
#' \code{knitr::kable()}, \code{kableExtra::kbl()}, or \code{xtable::xtable()}.
#'
#' @param x A \code{"cosmic_officer_summary"} object returned by
#'   \code{\link{officer_summary}}.
#' @param prob_outlier Probability cutoff used to flag an outlier. Default is
#'   \code{0.8}. Set to \code{0} to return all officers.
#'
#' @return A filtered data frame with class
#'   \code{c("cosmic_outlier_report", "data.frame")}.
#'
#' @examples
#' \dontrun{
#' fit <- cosmic(d, id, idOff, y)
#' off_summary <- officer_summary(fit)
#' outliers <- outlier_report(off_summary)
#' knitr::kable(outliers)
#' }
#'
#' @export
outlier_report <- function(x, prob_outlier = 0.8) {
  if (inherits(x, "cosmic_fit")) {
    stop(
      "x must be a 'cosmic_officer_summary' object. ",
      "Call officer_summary() first and pass the result to outlier_report().",
      call. = FALSE
    )
  }

  if (!inherits(x, "cosmic_officer_summary")) {
    stop("x must inherit from 'cosmic_officer_summary'", call. = FALSE)
  }

  if (!is.numeric(prob_outlier) || length(prob_outlier) != 1L ||
      is.na(prob_outlier) || prob_outlier < 0 || prob_outlier >= 1) {
    stop("prob_outlier must be a numeric scalar in [0, 1)",
         call. = FALSE)
  }

  required_cols <- c("idOff", "nPeers", "nInc", "pRankToppct",
                     "pRankBotpct", "lamMean", "lam025", "lam975")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("x is missing required columns: ",
         paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  force_cols <- grep("^force[0-9]+$", names(x), value = TRUE)
  keep_cols <- c("idOff", "nPeers", "nInc", force_cols,
                 "pRankToppct", "pRankBotpct",
                 "lamMean", "lam025", "lam975")

  keep_rows <- if (prob_outlier == 0) {
    rep(TRUE, nrow(x))
  } else {
    x$pRankToppct > prob_outlier | x$pRankBotpct > prob_outlier
  }

  keep_rows[is.na(keep_rows)] <- FALSE
  out <- x[keep_rows, keep_cols, drop = FALSE]

  if (nrow(out) > 0L) {
    out <- out[order(-out$pRankToppct, out$pRankBotpct), , drop = FALSE]
  }

  rownames(out) <- NULL
  class(out) <- c("cosmic_outlier_report", "data.frame")
  attr(out, "prob_outlier") <- prob_outlier

  return(out)
}
