library(cosmic)
library(xtable)

set.seed(20010618)

# -----------------------
# Simulate simple data
# -----------------------

nIncidents <- 100
nOfficers  <- 5

# latent officer propensities
lambda_true <- rnorm(nOfficers, 0, 1)

# ordinal scale
s <- c(0, 1, 1.5, 2)

# number of officers per incident
nOff <- sample(2:nOfficers, nIncidents, replace = TRUE)
# randomly assign officers to incidents
d <- data.frame(id=rep(1:nIncidents, nOff),
                idOff=unlist(sapply(nOff, function(x) sample(1:nOfficers, x))))

# translate lambda & s to probabilities of using type of force
p <- t(exp(s %*% t(lambda_true)))
p <- p/rowSums(p)
# simulate force used
d$p <- p[d$idOff,]
d$y <- apply(d$p, 1, function(x) sample(1:4, 1, prob=x))


# -----------------------
# Fit model
# -----------------------
fit <- cosmic(
  d,
  incidentID = id,
  officerID  = idOff,
  y          = y,
  iter       = 1000,
  chains     = 2,
  cores      = 1,
  threads    = 4)


# -----------------------
# Inspect results
# -----------------------
print(fit)

# summarize all officers relative to their peers
offSummary <- officer_summary(fit)
offSummary

# focus on officers with strong posterior evidence of being in the
# upper or lower tail of their peer group
outliers <- outlier_report(fit, prob_outlier = 0.8)
outliers

# optional table display for reports
xtable(
  outliers,
  digits = c(0, 0, 0, 0,
             rep(0, fit$data$nForceTypes),
             2, 2, 2, 2, 2)
) |>
  print(include.rownames = FALSE)
