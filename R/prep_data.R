#' Prepare data for COSMIC model
#'
#' Internal helper that validates inputs and constructs the data list
#' passed to Stan.
#'
#' @noRd
prep_cosmic_data <- function(data, incidentID, officerID, y) {
  incident_quo <- rlang::enquo(incidentID)
  officer_quo  <- rlang::enquo(officerID)
  y_quo        <- rlang::enquo(y)

  d <- data.frame(
    idOrig    = rlang::eval_tidy(incident_quo, data),
    idOffOrig = rlang::eval_tidy(officer_quo,  data),
    y         = rlang::eval_tidy(y_quo,        data)
  )

  idsWithInfo <- d |>
    dplyr::group_by(idOrig) |>
    dplyr::summarize(nUniqueY = dplyr::n_distinct(y), .groups = "drop") |>
    dplyr::filter(nUniqueY > 1) |>
    dplyr::select(idOrig)

  d <- d |>
    dplyr::semi_join(idsWithInfo, by = "idOrig") |>
    dplyr::mutate(
      id    = dplyr::dense_rank(idOrig),
      idOff = dplyr::dense_rank(idOffOrig)
    ) |>
    dplyr::arrange(id, idOff)

  # validation (your existing logic)
  if (!is.numeric(d$y)) stop("y must be numeric")
  if (any(is.na(d$y))) stop("y has NA")
  if (any(d$y != as.integer(d$y))) stop("y must be integer")

  y_vals <- sort(unique(d$y))
  if (!all(y_vals == seq_along(y_vals))) {
    stop("y must be 1..J consecutive")
  }

  if (max(d$y) < 3) {
    stop("Need at least 3 force types")
  }

  list(
    y           = as.integer(d$y),
    id          = as.integer(d$id),
    idOff       = as.integer(d$idOff),
    startIndex  = as.integer(which(!duplicated(d$id))),
    nMaxOffs    = max(table(d$id)),
    nRows       = nrow(d),
    nOff        = max(d$idOff),
    nIncidents  = max(d$id),
    nForceTypes = max(d$y)
  )
}
