#' Fit the Conditional Ordinal Stereotype Model (COSMIC)
#'
#' Fits the Conditional Ordinal Stereotype Model for Identification and Comparison (COSMIC)
#' to ordinal outcomes observed across multiple actors within shared events (e.g., officers
#' within incidents). The model uses a conditional likelihood to remove event-level
#' confounding and estimate actor-specific latent propensities relative to their peers.
#'
#' The likelihood is evaluated using a dynamic programming algorithm for the
#' Poisson-multinomial normalization term, enabling efficient computation for incidents
#' involving multiple actors. Posterior inference is performed via Markov chain Monte Carlo
#' using \pkg{cmdstanr}.
#'
#' @param data A data frame containing one row per actor-event observation
#' @param incidentID A column (unquoted) identifying the event or incident
#' @param officerID A column (unquoted) identifying the actor (e.g., officer)
#' @param y A column (unquoted) giving the ordinal outcome. Values must be consecutive
#'   integers starting at 1 (e.g., 1, 2, 3, ...)
#' @param priorSD_lambda Prior standard deviation for the actor-specific parameters
#'   \eqn{\lambda}. Default is 2
#' @param priorSD_sDiff Prior standard deviation for the differences between adjacent
#'   ordinal scale parameters
#' @param iter Number of MCMC iterations per chain
#' @param chains Number of Markov chains
#' @param cores Number of chains to run in parallel. When using within-chain
#'   parallelization (e.g., via \code{reduce_sum}), this should be set so that
#'   \code{cores * threads_per_chain} does not exceed the number of available CPU cores
#' @param threads Number of threads per chain used for within-chain parallelization
#'
#' @details
#' The COSMIC model is an ordinal regression model that estimates actor-specific latent
#' propensities while conditioning on the set of outcomes observed within each event.
#' This conditioning removes all event-level factors that are shared across actors,
#' allowing for comparisons that are invariant to environmental differences.
#'
#' The normalization term in the conditional likelihood involves a sum over permutations
#' consistent with the observed counts of outcome categories. This is computed using a
#' dynamic programming algorithm to avoid explicit enumeration.
#'
#' Parallel computation is supported both across chains and within chains. When specifying
#' \code{cores} and \code{threads}, users should ensure that total CPU usage remains within
#' hardware limits.
#'
#' @return
#' An object of class \code{"cosmic_fit"} containing:
#' \item{fit}{The fitted \code{CmdStanMCMC} object.}
#' \item{model}{The compiled \code{CmdStanModel} used for sampling.}
#' \item{data}{The processed data passed to Stan, including an
#'   \code{officer_lookup} table that maps sequential \code{idOff} values back
#'   to the original \code{officerID} values supplied by the user.}
#'
#' @examples
#' \dontrun{
#' d <- data.frame(
#'   id = c(1,1,2,2),
#'   idOff = c(1,2,1,2),
#'   y = c(1,2,1,3)
#' )
#'
#' fit <- cosmic(d, id, idOff, y,
#'               iter = 1000,
#'               chains = 2,
#'               cores = 1,
#'               threads = 4)
#'
#' print(fit)
#' }
#'
#' @seealso \code{\link{posterior}}, \code{\link{summary.cosmic_fit}}
#'
#' @export
cosmic <- function(data, incidentID, officerID, y,
                   priorSD_lambda = 2,
                   priorSD_sDiff  = 1,
                   iter = 2000,
                   chains  = 4,
                   cores   = 1,
                   threads = 8)
{
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(
      paste(
        "The 'cmdstanr' package is required to fit COSMIC models.",
        "Install it with install.packages('cmdstanr',",
        "repos = c('https://stan-dev.r-universe.dev', getOption('repos')))."
      ),
      call. = FALSE
    )
  }

  cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cmdstan_path) || !dir.exists(cmdstan_path)) {
    stop(
      "CmdStan is not installed. Run cmdstanr::install_cmdstan() before calling cosmic().",
      call. = FALSE
    )
  }

  message("Preparing data...")
  stanData <- prep_cosmic_data(data, {{incidentID}}, {{officerID}}, {{y}})

  stanData$rPriorSD_lambda <- priorSD_lambda
  stanData$rPriorSD_sDiff  <- priorSD_sDiff

  message("Setting initial values...")
  inits <- make_inits(stanData, chains)

  message("Loading conditional ordinal stereotype model in Stan...")
  path <- system.file("stan", "cosmic.stan", package = "cosmic")
  if (path == "") {
    stop("Bundled COSMIC Stan model not found in package.", call. = FALSE)
  }

  mod <- cmdstanr::cmdstan_model(
    stan_file = path,
    compile = TRUE,
    cpp_options = list(stan_threads = TRUE),
    quiet = TRUE)

  message("Sampling...")
  iter_warmup <- max(1L, floor(iter / 2))
  iter_sampling <- max(1L, iter - iter_warmup)
  stan_input <- stanData
  stan_input$officer_lookup <- NULL

  fit <- mod$sample(
    data = stan_input,
    init = inits,
    chains = chains,
    parallel_chains = max(1L, min(chains, cores)),
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    threads_per_chain = max(1L, as.integer(threads)),
    refresh = max(1L, floor(iter / 10))
  )

  structure(
    list(fit = fit, model = mod, data = stanData, backend = "cmdstanr"),
    class = "cosmic_fit"
  )
}
