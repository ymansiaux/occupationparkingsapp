#' Calcule les parametres rangeStart, rangeEnd et rangeStep pour xtradata
#'
#' @param selected_timestep pas de temps choisi par l'user (jour, semaine, mois, annee)
#' @param selected_date date selectionnee
#'
#' @return une liste de parametres
#' @export
#' @importFrom lubridate as_date add_with_rollback days floor_date weeks years ymd
occupation_compute_xtradata_request_parameters <- function(selected_timestep, selected_date) {
  if (selected_timestep == "Jour") {
    # jour
    rangeStart <- as_date(selected_date)
    rangeEnd <- add_with_rollback(rangeStart, days(1))
    rangeStep <- "hour"
  } else if (selected_timestep == "Semaine") {

    # semaine
    rangeStart <- floor_date(as_date(selected_date), "week", week_start = 1)
    rangeEnd <- floor_date(add_with_rollback(rangeStart, weeks(1)), "week", week_start = 1)
    rangeEnd <- as_date(ifelse(rangeEnd >= Sys.Date(), Sys.Date()-1, rangeEnd))
    rangeStep <- "day"
  } else if (selected_timestep == "Mois") {
    # mois
    rangeStart <- floor_date(as_date(selected_date), "month")
    rangeEnd <- add_with_rollback(rangeStart, months(1), roll_to_first = TRUE)
    rangeEnd <- as_date(ifelse(rangeEnd >= Sys.Date(), Sys.Date()-1, rangeEnd))
    rangeStep <- "day"
  } else if (selected_timestep == "Ann\u00e9e") {

    # annee
    rangeStart <- ymd(paste0(selected_date, "0101"))
    rangeEnd <- add_with_rollback(rangeStart, years(1), roll_to_first = TRUE)
    rangeEnd <- as_date(ifelse(rangeEnd >= Sys.Date(), Sys.Date()-1, rangeEnd))
    if(rangeStart == "2020-01-01") rangeStart <- as.Date("2020-04-01")
    rangeStep <- "month"
  }

  list("rangeStart" = rangeStart, "rangeEnd" = rangeEnd, "rangeStep" = rangeStep)
}
