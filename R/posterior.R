#' Extract posterior draws from a COSMIC model
#'
#' Extracts posterior samples from a fitted COSMIC model. This is a
#' convenience wrapper around \code{rstan::extract()} that avoids direct
#' dependence on \pkg{rstan} in user code.
#'
#' @param object A fitted object of class \code{"cosmic_fit"}.
#' @param ... Additional arguments passed to \code{rstan::extract()}.
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
#'   as in \code{rstan::extract()}. If \code{TRUE}, returns a data frame
#'   with one row per draw.
#' @export
posterior.cosmic_fit <- function(object, pars = NULL, tidy = FALSE, ...) {

  draws <- rstan::extract(object$fit, pars = pars, ...)

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
