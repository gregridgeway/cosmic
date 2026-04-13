library(cosmic)

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

# extract posterior draws
draws <- posterior(fit)

# posterior means of lambda
#   only differences are identifiable
#   compare with lambda[1]
lambda_post_mean <- colMeans(draws$lambda - draws$lambda[,1])

# compare to truth (up to scale/shift)
plot(lambda_true-lambda_true[1],
     lambda_post_mean,
     xlab = "True lambda",
     ylab = "Estimated lambda",
     main = "COSMIC recovery (relative scale)")
abline(0, 1, col = "red")

# -----------------------
# Rank officers
# -----------------------

# simple ranking by posterior mean
rank_est <- order(lambda_post_mean)

head(rank_est)
