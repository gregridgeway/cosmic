#' Load the packaged COSMIC Stan model
#'
#' The Stan model is compiled as part of package installation using the
#' \pkg{rstantools} scaffolding and exposed at runtime through the generated
#' \code{stanmodels} list in \file{R/stanmodels.R}. This helper returns the
#' installed model object for sampling.
#'
#' @noRd
.get_stan_model <- function() {
  if (!exists("stanmodels", inherits = TRUE)) {
    stop("Packaged COSMIC Stan model is not available.", call. = FALSE)
  }

  model <- get("stanmodels", inherits = TRUE)[["cosmic"]]

  if (!inherits(model, "stanmodel")) {
    stop("Packaged COSMIC Stan model is not available.", call. = FALSE)
  }

  model
}
