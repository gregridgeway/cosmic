library(cosmic)
library(xtable)

# -----------------------
# Simulate simple data
# -----------------------

# nIncidents <- 100
# nOfficers  <- 5
#
# # latent officer propensities
# lambda_true <- rnorm(nOfficers, 0, 1)
#
# # ordinal scale
# s <- c(0, 1, 1.5, 2)
#
# # number of officers per incident
# nOff <- sample(2:nOfficers, nIncidents, replace = TRUE)
# # randomly assign officers to incidents
# d <- data.frame(id=rep(1:nIncidents, nOff),
#                 idOff=unlist(sapply(nOff, function(x) sample(1:nOfficers, x))))
#
# # translate lambda & s to probabilities of using type of force
# p <- outer(lambda_true, s) |> exp()
# p <- p/rowSums(p)
# # simulate force used
# d$p <- p[d$idOff,]
# d$y <- apply(d$p, 1, function(x) sample(1:4, 1, prob=x))


# 3+3 disconnected, 250 incidents per officers --------------------------------
set.seed(20010618)
nIncidents <- 2000
nOff <- sample(2:3, nIncidents, replace = TRUE)
d1 <- data.frame(id    = rep(1:nIncidents, nOff),
                 idOff = sapply(nOff, function(x) sample(1:3, x)) |> unlist(),
                 group = 1)
nOff <- sample(2:3, nIncidents, replace = TRUE)
d2 <- data.frame(id    = rep(nIncidents + 1:nIncidents, nOff),
                 idOff = sapply(nOff, function(x) sample(4:6, x)) |> unlist(),
                 group = 2)
d <- rbind(d1,d2)

lambda <- -c(0.5, 1, 2, 0, 0.75, 1.5)
s <- c(0, 1, 1.5, 2)

p <- outer(lambda, s) |> exp()
p <- p/rowSums(p)

d$p <- p[d$idOff,]
d$y <- apply(d$p, 1, function(x) sample(1:4, 1, prob=x))
d <- d[,c("id","idOff","y")]

# check empirical probabilities resemble p
p
aggregate(cbind(y==1,y==2,y==3,y==4)~idOff, data=d, mean)



# -----------------------
# Fit model
# -----------------------
# fit <- cosmic(
#   d,
#   incidentID = id,
#   officerID  = idOff,
#   y          = y,
#   iter       = 10000,
#   chains     = 4,
#   cores      = 1,
#   threads    = 8)
# save(fit, file = "basic_demo_cosmic_fit.RData")

message(
  "Loading the precomputed demo fit from 'basic_demo_cosmic_fit.RData' ",
  "instead of running `cosmic()`, which takes substantially longer."
)
fit_path <- system.file("demo", "basic_demo_cosmic_fit.RData", package = "cosmic")
if (!nzchar(fit_path)) {
  fit_path <- file.path("demo", "basic_demo_cosmic_fit.RData")
}
load(fit_path)


# -----------------------
# Inspect results
# -----------------------
print(fit)

diagnose(fit)

# summarize all officers relative to their peers
flag_officer_tail_pct <- 0.05
offSummary <- officer_summary(fit, pct_tail = flag_officer_tail_pct)
offSummary

# focus on officers with strong posterior evidence of being in the
# upper or lower tail of their peer group
outliers <- outlier_report(offSummary, prob_outlier = 0.1)
outliers

# optional table display for reports
xtable(outliers,
       digits = c(0, 0, 0, 0,
                  rep(0, fit$data$nForceTypes),
                  2, 2, 2, 2, 2)) |>
  print(include.rownames = FALSE)

if (requireNamespace("knitr", quietly = TRUE) &&
    requireNamespace("kableExtra", quietly = TRUE)) {

  outliers_display <- outliers
  prob_cols <- names(outliers_display) %in% c("pRankToppct", "pRankBotpct")
  tail_pct_label <- formatC(100 * flag_officer_tail_pct,
                            format = "fg", digits = 3)
  names(outliers_display)[names(outliers_display) == "pRankToppct"] <-
    paste0("Prob. top ", tail_pct_label, "%")
  names(outliers_display)[names(outliers_display) == "pRankBotpct"] <-
    paste0("Prob. bottom ", tail_pct_label, "%")
  outliers_display[prob_cols] <-
    lapply(outliers_display[prob_cols],
           function(x) sprintf("%.2f", x))

  digits <- rep(0, ncol(outliers_display))
  digits[seq.int(length(digits)-4,length(digits))] <- 2L

  knitr::kable(
    outliers_display,
    format = "html",
    digits = digits,
    caption = "Officers with strong posterior evidence of being in the tails of the peer distribution",
    align = rep("r", ncol(outliers_display))) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center") |>
    kableExtra::row_spec(0, bold = TRUE, color = "white",
                         background = "#2C3E50") |>
    kableExtra::column_spec(1, bold = TRUE) |>
    kableExtra::scroll_box(width = "100%", height = "320px")
} else {
  message("Install 'kableExtra' to render the styled outlier table.")
}
