#' R6 Class pour donnees occupations des parkings
#'
#' @description
#' va gérer la routine API / clean / plot / table / download
Saturation <- R6::R6Class(
  "Saturation",
  
  public = list(
    #' @field rangeStart Debut de la periode d'observation
    rangeStart = "",
    
    #' @field rangeEnd Fin de la periode d'observation
    rangeEnd = "",
    
    #' @field localisation_parking Secteur de localisation du parking (hypercentre, centre, peripherie, NA pour les parc relais)
    localisation_parking = "",
    
    #' @field parc_relais Parc relais ou non (boolean)
    parc_relais = "",
    
    #' @field data_xtradata Données issues de l'appel au WS via la fonction download_data
    data_xtradata = NULL,
    
    #' @field parkings_satures contient les parkings satures selon des criteres de % 
    #' d'occupation en nb d'heures par jour et nb de jours par semaine
    parkings_satures = NULL,
    
    #' @description
    #' Create a new occupation object.
    #' @param rangeStart rangeStart
    #' @param rangeEnd rangeEnd.
    #' @param localisation_parking localisation_parking
    #' @param parc_relais parc_relais
    #' @param data_xtradata data_xtradata
    #' @return A new `Occupation` object.
    
    initialize = function(rangeStart, rangeEnd, localisation_parking, parc_relais) {
      self$rangeStart <- rangeStart
      self$rangeEnd <- rangeEnd
      self$localisation_parking <- localisation_parking
      self$parc_relais <- parc_relais
      self$data_xtradata <- NULL
      self$parkings_satures <- NULL
    },
    
    #' @description
    #' Interroge le WS aggregate
    #' @param rangeStep rangeStep xtradata aggregate
    #' @import tidytable
    #' @importFrom dplyr pull
    #' @importFrom xtradata xtradata_requete_aggregate
    #' @examples \dontrun{
    #' parc_relais <- Occupation(rangeStart = Sys.Date() - 2, 
    #' rangeEnd = Sys.Date() - 1, localisation_parking = NA, parc_relais = TRUE)
    #' parc_relais$download_data()
    #' parc_relais$data_xtradata }
    download_data = function(rangeStep) {
      self$data_xtradata <- try(xtradata_requete_aggregate(
        key = "DATAZBOUBB",
        typename = "ST_PARK_P",
        rangeStart = self$rangeStart,
        rangeEnd = self$rangeEnd,
        rangeStep = "hour",
        rangeFilter = list(hours = 0:23, days = 1:7, publicHolidays = FALSE),
        filter = list(
          "ident" =
            list(
              "$in" =
                parkings %>% filter.(localisation_parking %in% self$localisation_parking & parc_relais == self$parc_relais) %>% select.(ident) %>% pull()
            )
        ),
        attributes = list("gid", "time", "libres", "total", "etat", "ident"),
        showURL = TRUE
      ))
    },
    
    #' @description
    #' Nettoyage de la sortie xtradata
    #' (application de lubridate et calcul du taux d'occup)
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom lubridate as_datetime
    #' @examples \dontrun{ clean_output()
    
    #' }
    clean_output = function() {
      ## rajouter du defensive programming
      self$data_xtradata <- self$data_xtradata %>%
        select.(-type) %>%
        mutate.(time = as_datetime(time),
                libres = ceiling(libres),
                taux_occupation = pmax(0, 1-(libres / total)))
    },
    
    #' @description
    #' On garde les parkings satures. Càd les parkings avec un taux d'occupation
    #' superieur à seuil_taux_occupation pendant au moins nb_heures_par_jour_satures 
    #' par jour pendant au moins nb_jour_par_semaine_sature par semaine
    #' @param seuil_taux_occupation seuil pour considerer un parking comme sature
    #' @param nb_heures_par_jour_satures seuil de nb d'heures par jour de saturation
    #' @param nb_jour_par_semaine_sature seuil de nb de jour par semaine de saturation
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom lubridate as_date floor_date
    #' @examples \dontrun{ temporal_aggregate("day")
    #' } 
    filter_full_capacity_parkings = function(seuil_taux_occupation = .9, nb_heures_par_jour_satures = 3, nb_jour_par_semaine_sature = 2) {
      
      # calcul pour chaque parking du nombre d'heure / j pdt lequel il est sature
      n_hour_per_day_full <- self$data_xtradata %>% 
        mutate.(date = as_date(time)) %>% 
        summarise.(n_hour_per_day_full = sum(taux_occupation >= seuil_taux_occupation), .by = c(ident, date)) 
      
      # calcul pour chaque parking du nombre de jours par semaine pendant lequel il est sature au moins `nb_heures_par_jour_satures` heures
      n_days_per_week_full <- n_hour_per_day_full %>% 
        mutate.(week = floor_date(date, unit = "week", week_start = 1)) %>%
        summarise.(n_day_per_week_full = sum(n_hour_per_day_full >= nb_heures_par_jour_satures), .by = c(ident, week)) 
      
      # on converse les parkings qui remplissent le critere de saturation journaliere au moins `nb_jour_par_semaine_sature` jours dans une semaine
      self$parkings_satures <- n_days_per_week_full %>% 
        filter.(n_day_per_week_full >= nb_jour_par_semaine_sature)
      
    },
    
    #' @description
    #' Realise une calendar heatmap des parkings les plus satures
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom ggplot2 ggplot aes geom_tile scale_fill_distiller scale_x_continuous facet_grid theme_minimal theme unit element_blank coord_equal
    #' @importFrom lubridate hour wday
    #' @examples \dontrun{ temporal_aggregate("day")
    #' } 
    calendar_heatmap = function() {
      
      data_parkings_heatmap <- self$data_xtradata %>% 
        inner_join.(self$parkings_satures, by = "ident") %>% 
        mutate.(hours = hour(time), days = wday(time, label = TRUE), taux = taux_occupation * 100) 
      
      ggplot(data_parkings_heatmap, aes(hours, days)) +
        geom_tile(aes(fill = taux), colour = "white") +
        scale_fill_distiller(palette = "Spectral", direction = -1) +
        scale_x_continuous(breaks = 0:23) +
        facet_grid(ident ~ .) + 
        theme_minimal() + 
        theme(
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          panel.grid = element_blank()
        ) +
        coord_equal()
      
    }
  )
)
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html