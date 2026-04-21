#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import methods
#' @import Rcpp
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom stats sd runif
## usethis namespace: end
#' @useDynLib cosmic, .registration = TRUE
NULL

utils::globalVariables(c(
  "idOrig", "idOffOrig", "idOff", "id",
  "nUniqueY"
))
