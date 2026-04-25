#' Generate initial values for COSMIC model parameters
#'
#' Internal helper that constructs a list of initial values for the
#' Markov chains used in Stan backends. Multiple initialization strategies
#' are used to improve robustness of sampling and reduce sensitivity to
#' starting values.
#'
#' The function returns a list of length \code{chains},
#' where each element is a named list containing initial values for:
#' \itemize{
#'   \item \code{lambda}: actor-specific latent parameters
#'   \item \code{sDelta}: differences between adjacent ordinal scale parameters
#' }
#'
#' Initialization strategies include:
#' \enumerate{
#'   \item All-zero initialization for \code{lambda} with evenly spaced
#'         \code{sDelta}, corresponding to a linear ordinal scale where
#'         \eqn{s = (0, 1, 2)} for three force types and generalizing to
#'         equally spaced values for larger outcome sets
#'   \item Discrete initialization for \code{lambda} drawn from \{-1, 0, 1\}
#'         with the same evenly spaced \code{sDelta}
#'   \item Random initialization with \code{lambda} drawn from a uniform
#'         distribution and \code{sDelta} drawn independently from a positive
#'         range, allowing non-linear spacing of the ordinal scale
#'   \item Data-informed initialization where \code{lambda} is based on
#'         standardized mean outcomes by actor, paired with evenly spaced
#'         \code{sDelta}
#' }
#'
#' The evenly spaced \code{sDelta} values are constructed so that the initial
#' values of \eqn{s} starts with 0 and 1 and then increase linearly, reaching 2
#' at the highest category. The random initialization of \code{sDelta}
#' introduces variability in the spacing of the ordinal scale while preserving
#' monotonicity through strictly positive increments.
#'
#' @param stanData A list containing the processed data passed to Stan.
#'   Must include elements \code{y}, \code{idOff}, \code{nOff}, and
#'   \code{nForceTypes}
#'
#' @param chains Number of Markov chains, corresponding to the number of sets
#'   of initial values to generate
#'
#' @return A list of length equal to the number of chains, where each element
#'   is a named list of initial values compatible with Stan sampling interfaces
#'
#' @details
#' Good initialization can substantially improve convergence and reduce
#' divergent transitions in hierarchical and non-linear models. The multiple
#' strategies implemented here are intended to provide diverse starting points
#' across chains, which can help diagnose multimodality or weak identifiability.
#'
#' The \code{sDelta} parameters are constrained to be positive, ensuring that
#' the ordinal scale remains strictly increasing.
#'
#' @noRd
make_inits <- function(stanData, chains = 4) {

  if(chains < 1) stop("chains must be greater than or equal to 1")

  # so that init s is 0, 1, equally spaced values up to 2
  sDeltaInit <- rep(1 / (stanData$nForceTypes - 2),
                    times = stanData$nForceTypes - 2)

  a <- tapply(stanData$y, stanData$idOff, mean)
  if (sd(a) > 0) {
    a <- (a - mean(a)) / sd(a)
  } else { # shouldn't get here
    a <- rep(0, length(a))
  }

  inits <- list()

  inits[[1]] <- list(lambda = rep(0, stanData$nOff),
                     sDelta = as.vector(sDeltaInit))
  if(chains > 1)
  {
    inits[[2]] <- list(lambda = sample(c(-1,0,1), stanData$nOff, TRUE),
                       sDelta = sDeltaInit)
  }
  if(chains > 2)
  {
    inits[[3]] <- list(lambda = runif(stanData$nOff, -2, 2),
                       sDelta = runif(stanData$nForceTypes-2, 0.05, 1))
  }
  if(chains > 3)
  {
    inits[[4]] <- list(lambda = a, sDelta = sDeltaInit)
  }
  if(chains > 4)
  {
    for(i in seq(5, chains))
    {
      inits[[i]] <- list(lambda = runif(stanData$nOff, -2, 2),
                         sDelta = runif(stanData$nForceTypes-2, 0.05, 1))
    }
  }

  for(i in seq_len(chains))
  {
    inits[[i]]$sDelta <- array(inits[[i]]$sDelta, dim=stanData$nForceTypes-2)
  }

  return(inits)
}
