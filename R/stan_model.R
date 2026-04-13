.stan_model_cache <- NULL

#' Compile and cache Stan model
#'
#' Internal helper that locates the Stan file and compiles it.
#'
#' @noRd
.get_stan_model <- function()
{
  if (!is.null(.stan_model_cache)) return(.stan_model_cache)

  path <- system.file("stan", "cosmic.stan", package = "cosmic")
  .stan_model_cache <<- rstan::stan_model(path)

  return(.stan_model_cache)
}
