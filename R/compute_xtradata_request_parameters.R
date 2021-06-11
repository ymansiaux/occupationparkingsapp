#' Calcule les parametres rangeStart, rangeEnd et rangeStep pour xtradata
#'
#' @param selected_timestep pas de temps choisi par l'user (jour, semaine, mois, annee)
#'
#' @return une liste de parametres
#' @export
#' @importFrom clock as_date add_days add_weeks add_months date_build add_years get_year date_group
#' @examples occupation_compute_xtradata_request_parameters("Jour", "2021-04-01")
occupation_compute_xtradata_request_parameters <- function(selected_timestep, selected_date) {
  
  if(selected_timestep == "Jour") {
    # jour
    rangeStart <- as_date(selected_date)
    rangeEnd <- add_days(rangeStart, 1)
    rangeStep <- "hour"
  } else if(selected_timestep == "Semaine") {
    
    # semaine
    rangeStart <- as_date(selected_date)
    rangeEnd <- add_weeks(rangeStart, 1)
    rangeEnd <- as_date(ifelse(rangeEnd > Sys.Date(), Sys.Date(), rangeEnd))
    rangeStep <- "day"
    
  } else if(selected_timestep == "Mois") {
    # mois
    rangeStart <- date_group(as_date(selected_date), "month")
    rangeEnd <- add_months(rangeStart, 1)
    rangeEnd <- as_date(ifelse(rangeEnd > Sys.Date(), Sys.Date(), rangeEnd))
    rangeStep <- "day"
    
  } else if(selected_timestep == "Année") {
    
    # annee
    rangeStart <- date_build(as.numeric(selected_date),1,1)
    rangeEnd <- add_years(rangeStart, 1)
    rangeEnd <- as_date(ifelse(rangeEnd > Sys.Date(), Sys.Date(), rangeEnd))
    rangeStep <- "month"
    
  }
  
  list("rangeStart" = rangeStart, "rangeEnd" = rangeEnd, "rangeStep" = rangeStep)
  
}