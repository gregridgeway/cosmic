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
    model <- tryCatch(readRDS(rds_path), error = function(e) NULL)
    if (.stan_model_is_valid(model)) {
      return(model)
    }
  }

  path <- system.file("stan", "cosmic.stan", package = "cosmic")

  if (path == "") {
    stop("Bundled COSMIC Stan model not found in package.", call. = FALSE)
  }

  rstan::rstan_options(auto_write = TRUE)
  rstan::stan_model(file = path, model_name="CondOrdStereoModel")
}

.stan_model_is_valid <- function(model) {
  if (!inherits(model, "stanmodel")) {
    return(FALSE)
  }

  # Serialized stanmodel objects can carry a compiled DSO that is invalid on a
  # different platform or R session. Probe the module before handing it to
  # rstan::sampling(), and fall back to recompiling from the Stan source when
  # the serialized object cannot be used.
  isTRUE(tryCatch({
    model@mk_cppmodule(model)
    TRUE
  }, error = function(e) FALSE))
}
