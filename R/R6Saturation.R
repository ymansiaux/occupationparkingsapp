#' R6 Class pour donnees occupations des parkings
#'
#' @description
#' va gérer la routine API / clean / plot / table / download
Saturation <- R6::R6Class(
  "Saturation",
  inherit = ParkingsStats,
  
  public = list(
    
    #' @field parkings_satures contient les parkings satures selon des criteres de % 
    #' d'occupation en nb d'heures par jour et nb de jours par semaine
    parkings_satures = NULL,
    
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
    filter_full_capacity_parkings = function(seuil_taux_occupation = 90, nb_heures_par_jour_satures = 3, nb_jour_par_semaine_sature = 2) {
      
      # calcul pour chaque parking du nombre d'heure / j pdt lequel il est sature
      n_hour_per_day_full <- self$cleaned_data %>% 
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
    #' @importFrom ggplot2 ggplot ggtitle aes geom_tile scale_fill_distiller scale_y_continuous scale_x_date facet_wrap theme_minimal theme unit element_blank coord_equal element_text
    #' @importFrom lubridate hour wday
    #' @importFrom ggiraph geom_tile_interactive
    #' @importFrom glue glue_data
    #' @import ggiraph
    #' @import ggplot2
    #' @examples \dontrun{ temporal_aggregate("day")
    #' } 
    calendar_heatmap = function(with_facet = FALSE, selected_parking) {
      
      data_parkings_heatmap <- self$cleaned_data %>% 
        inner_join.(self$parkings_satures, by = "ident") %>% 
        filter.(ident == selected_parking) %>% 
        mutate.(hours = hour(time), date = as_date(time)) %>% 
        mutate.(tooltip = glue_data(.SD, "Date : {as.character(time)}\nTaux : {sprintf('%.2f', taux_occupation)}"))
      
      gg <- ggplot(data_parkings_heatmap, aes(y = date, x = hours, tooltip = tooltip)) +
        geom_tile_interactive(aes(fill = taux_occupation), colour = "white") +
        scale_fill_distiller(palette = "Spectral", direction = -1) +
        scale_x_continuous(breaks = 0:23) +
        scale_y_date(date_labels = "%d/%m", breaks = "3 days", expand = c(0,0)) +
        ggtitle(data_parkings_heatmap$nom[1]) + 
        # facet_wrap(~ nom, ncol = 2, strip.position = "top") +
        theme_minimal() + 
        theme(
          legend.position = "bottom",
          # legend.key.width = unit(2, "cm"),
          text = element_text(size=16),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        ) +
        coord_equal()
      
      if(with_facet) gg <- gg + facet_wrap(~ nom, ncol = 2, strip.position = "top")
      
      gg

    }
  )
)


#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html