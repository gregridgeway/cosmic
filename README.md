# cosmic

`cosmic` is an R package implementing the **COSMIC** model: the
Conditional Ordinal Stereotype Model for Incident-level Calibration.

## Installation

```r
# install.packages("pak")
pak::pak("path/to/cosmic")
```

## Minimal example

```r
library(cosmic)

set.seed(42)
n <- 100
x <- rnorm(n)
eta <- 0.6 * x
y <- as.integer(cut(eta + rnorm(n),
  breaks = quantile(eta + rnorm(n), probs = c(0, .33, .66, 1)),
  include.lowest = TRUE
))

fit <- cosmic_mcmc(y ~ x, data = data.frame(y = y, x = x), iter = 500, burnin = 100)
print(fit)
summary(fit)
```
