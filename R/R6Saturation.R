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
    #' @importFrom ggiraph girafe opts_sizing opts_tooltip opts_hover geom_tile_interactive
    #' @examples \dontrun{ temporal_aggregate("day")
    #' } 
    calendar_heatmap = function() {
      
      data_parkings_heatmap <- self$data_xtradata %>% 
        inner_join.(self$parkings_satures, by = "ident") %>% 
        mutate.(hours = hour(time), days = wday(time, label = TRUE), taux = taux_occupation * 100) 
      
      gg <- ggplot(data_parkings_heatmap, aes(x = hours, y = days, tooltip = taux)) +
        geom_tile_interactive(aes(fill = taux), colour = "white") +
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
      
      girafe(
        ggobj = gg, width_svg = 6, height_svg = 6,
        options = list(
          opts_sizing(rescale = FALSE),
          opts_tooltip(
            opacity = .8,
            css = "background-color:gray;color:white;padding:2px;border-radius:2px;"
          ),
          opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
        )
      )
      
    }
  )
)


#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html