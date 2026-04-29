#' @details
#' Fitting COSMIC models requires the optional \pkg{cmdstanr} package and a local
#' CmdStan installation. If these are not already installed, run:
#' \preformatted{
#' install.packages("cmdstanr",
#'                  repos = c("https://stan-dev.r-universe.dev",
#'                            getOption("repos")))
#' cmdstanr::install_cmdstan()
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats sd runif
## usethis namespace: end
NULL

utils::globalVariables(c(
  "idOrig", "idOffOrig", "idOff", "id",
  "nUniqueY"
))
