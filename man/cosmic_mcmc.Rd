\name{cosmic_mcmc}
\alias{cosmic_mcmc}
\title{Fit the COSMIC model via MCMC}
\usage{
cosmic_mcmc(
  formula,
  data,
  iter = 2000,
  burnin = floor(iter/2),
  step_size = 0.1,
  prior_sd = 5
)
}
\arguments{
\item{formula}{A two-category outcome formula.}
\item{data}{Data frame containing model variables.}
\item{iter}{Total MCMC iterations.}
\item{burnin}{Number of burn-in iterations.}
\item{step_size}{Proposal standard deviation for random-walk MH.}
\item{prior_sd}{Prior standard deviation.}
}
\value{An object of class \code{cosmic_fit}.}
\description{Fits a COSMIC model using a Markov chain Monte Carlo routine.}
