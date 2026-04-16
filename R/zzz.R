.onLoad <- function(libname, pkgname)
{
  # enable rstan model caching
  if (requireNamespace("rstan", quietly = TRUE)) {
    rstan::rstan_options(auto_write = TRUE)
  }
}
