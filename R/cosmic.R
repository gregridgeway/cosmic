#' Fit Conditional Ordinal Stereotype Model
#'
#' @export
cosmic <- function(data, incidentID, officerID, y,
                   priorSD_lambda = 2,
                   priorSD_sDiff  = 1,
                   iter = 2000,
                   chains = 4,
                   threads = 2) {

  stanData <- prep_cosmic_data(data, {{incidentID}}, {{officerID}}, {{y}})

  stanData$rPriorSD_lambda <- priorSD_lambda
  stanData$rPriorSD_sDiff  <- priorSD_sDiff

  inits <- make_inits(stanData)

  Sys.setenv(STAN_NUM_THREADS = threads)

  mod <- .get_stan_model()

  fit <- rstan::sampling(
    mod,
    data   = stanData,
    init   = inits,
    iter   = iter,
    chains = chains,
    cores  = threads)

  structure(
    list(fit = fit, data = stanData),
    class = "cosmic_fit"
  )
}
