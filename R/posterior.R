#' Extract posterior draws from a COSMIC model
#'
#' Extracts posterior samples from a fitted COSMIC model. This is a
#' convenience wrapper around CmdStan draws extraction that avoids direct
#' dependence on \pkg{cmdstanr} in user code.
#'
#' @param object A fitted object of class \code{"cosmic_fit"}.
#' @param ... Additional arguments passed to \code{object$fit$draws()}.
#'
#' @return
#' If \code{tidy = FALSE}, a named list of posterior draws.
#'
#' If \code{tidy = TRUE}, a data frame with one row per posterior draw and
#' columns corresponding to parameters (expanded with indices where needed).
#'
#' @examples
#' \dontrun{
#' fit <- cosmic(d, id, idOff, y)
#'
#' # raw posterior draws
#' draws <- posterior(fit)
#'
#' # only lambda
#' lambda_draws <- posterior(fit, pars = "lambda")
#'
#' # tidy format
#' df <- posterior(fit, tidy = TRUE)
#' }
#'
#' @export
posterior <- function(object, ...) {
  UseMethod("posterior")
}


#' @rdname posterior
#' @param pars Optional character vector of parameter names to extract
#'   (e.g., \code{"lambda"}, \code{"sDelta"}). Defaults to all parameters.
#' @param tidy Logical; if \code{FALSE} (default), returns a list of arrays
#'   as a named list of posterior draw arrays. If \code{TRUE}, returns a data frame
#'   with one row per draw.
#' @export
posterior.cosmic_fit <- function(object, pars = NULL, tidy = FALSE, ...) {
  draws_matrix <- posterior::as_draws_matrix(
    object$fit$draws(variables = pars, format = "draws_matrix", ...)
  )
  draws <- .draws_matrix_to_list(draws_matrix)

  if (!tidy) {
    return(draws)
  }

  # flatten arrays into columns
  out <- list()

  for (name in names(draws)) {

    x <- draws[[name]]

    if (is.null(dim(x))) {
      # scalar parameter
      out[[name]] <- x

    } else if (length(dim(x)) == 2) {
      # vector parameter: iterations x K
      for (j in seq_len(ncol(x))) {
        out[[paste0(name, "[", j, "]")]] <- x[, j]
      }

    } else if (length(dim(x)) == 3) {
      # matrix parameter: iterations x K x L
      for (j in seq_len(dim(x)[2])) {
        for (k in seq_len(dim(x)[3])) {
          out[[paste0(name, "[", j, ",", k, "]")]] <- x[, j, k]
        }
      }
    }
  }

  as.data.frame(out)
}

.draws_matrix_to_list <- function(draws_matrix) {
  if (is.null(colnames(draws_matrix)) || !length(colnames(draws_matrix))) {
    return(list())
  }

  variable_names <- colnames(draws_matrix)
  base_names <- unique(sub("\\[.*$", "", variable_names))

  out <- stats::setNames(vector("list", length(base_names)), base_names)

  for (base_name in base_names) {
    keep <- sub("\\[.*$", "", variable_names) == base_name
    cols <- which(keep)
    col_names <- variable_names[cols]

    scalar_col <- match(base_name, col_names, nomatch = 0L)
    if (length(cols) == 1L && scalar_col == 1L) {
      out[[base_name]] <- as.numeric(draws_matrix[, cols])
      next
    }

    index_strings <- sub(
      paste0("^", .regex_escape(base_name), "\\[([^]]+)\\]$"),
      "\\1",
      col_names
    )
    index_matrix <- do.call(
      rbind,
      strsplit(index_strings, ",", fixed = TRUE)
    )
    storage.mode(index_matrix) <- "integer"

    dims <- apply(index_matrix, 2, max)
    param_matrix <- draws_matrix[, cols, drop = FALSE]

    if (length(dims) == 1L) {
      arr <- array(NA_real_, dim = c(nrow(param_matrix), dims))
      for (j in seq_along(cols)) {
        arr[, index_matrix[j, 1]] <- param_matrix[, j]
      }
    } else if (length(dims) == 2L) {
      arr <- array(NA_real_, dim = c(nrow(param_matrix), dims[1], dims[2]))
      for (j in seq_along(cols)) {
        arr[, index_matrix[j, 1], index_matrix[j, 2]] <- param_matrix[, j]
      }
    } else {
      stop(
        sprintf(
          "Posterior extraction only supports parameters with up to 2 indices. '%s' has %d.",
          base_name,
          length(dims)
        ),
        call. = FALSE
      )
    }

    out[[base_name]] <- arr
  }

  out
}

.regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}
