library(cosmic)
library(xtable)



# load Seattle data --------------------------------
path <- system.file("demo", "dataSPD.RData", package = "cosmic")
load(path)


# -----------------------
# Fit model
# -----------------------
# Can take several hours (~4 hours)
# fit <- cosmic(
#   d,
#   incidentID = id,
#   officerID  = idOff,
#   y          = y,
#   iter       = 10000,
#   chains     = 4,
#   cores      = 1,
#   threads    = 8)
# save(fit, file = "SPD_fit.RData")

message(
  "Loading the precomputed demo fit from 'SPD_fit.RData' ",
  "instead of running `cosmic()`, which takes substantially longer."
)
fit_path <- system.file("demo", "SPD_fit.RData", package = "cosmic")
if (!nzchar(fit_path)) {
  fit_path <- file.path("demo", "SPD_fit.RData")
}
load(fit_path)


# -----------------------
# Inspect results
# -----------------------
print(fit)

diagnose(fit)

# summarize all officers relative to their peers
future::plan(future::multisession, workers = 4)
flag_officer_tail_pct <- 0.05
offSummary <- officer_summary(fit, pct_tail = flag_officer_tail_pct)
offSummary

# focus on officers with strong posterior evidence of being in the
# upper or lower tail of their peer group
outliers <- outlier_report(offSummary, prob_outlier = 0.9)
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
