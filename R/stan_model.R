#' Load the COSMIC Stan model
#'
#' Internal helper that locates the bundled Stan model. When a serialized
#' \code{stanmodel} object is available it is loaded directly, avoiding a fresh
#' compilation during tests and routine package use. Otherwise the Stan source
#' file is compiled with \pkg{rstan}. Compilation is cached automatically by
#' \pkg{rstan} when \code{auto_write = TRUE}.
#'
#' @noRd
.get_stan_model <- function() {
  rds_path <- system.file("stan", "cosmic.rds", package = "cosmic")
  if (rds_path != "" && file.exists(rds_path)) {
    return(readRDS(rds_path))
  }

  path <- system.file("stan", "cosmic.stan", package = "cosmic")

  if (path == "") {
    stop("Bundled COSMIC Stan model not found in package.", call. = FALSE)
  }

  rstan::stan_model(file = path, model_name="CondOrdStereoModel")
}
