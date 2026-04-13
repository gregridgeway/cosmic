#' @export
print.cosmic_fit <- function(x, ...) {
  print(x$fit, pars = c("lambda", "sDelta"))
}

#' @export
summary.cosmic_fit <- function(object, ...) {
  summary(object$fit)$summary
}
