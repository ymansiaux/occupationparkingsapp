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

    #' @field data_plot donnees du graphique de saturation
    data_plot = NULL,
    
    #' @description
    #' Create a new saturation object.
    #' @param rangeStart rangeStart
    #' @param rangeEnd rangeEnd
    #' @param rangeStep rangeStep
    #' @param timeStep timeStep
    #' @param plageHoraire plageHoraire
    #' @param parkings_list liste des parkings analyses
    #' @return A new `Saturation` object.
    
    initialize = function(rangeStart = NULL, rangeEnd = NULL, rangeStep = NULL, timeStep = NULL, plageHoraire = NULL, parkings_list = NULL) {
      super$initialize(rangeStart, rangeEnd, rangeStep, timeStep, plageHoraire, parkings_list)
    },

    #' @description
    #' On garde les parkings satures. Càd les parkings avec un taux d'occupation
    #' superieur à seuil_taux_occupation pendant au moins nb_heures_par_jour_satures
    #' par jour pendant au moins nb_jour_par_semaine_sature par semaine
    #' @param seuil_taux_occupation seuil pour considerer un parking comme sature
    #' @param nb_heures_par_jour_satures seuil de nb d'heures par jour de saturation
    #' @param nb_jour_par_semaine_sature seuil de nb de jour par semaine de saturation
    #' @import data.table
    #' @importFrom lubridate as_date floor_date

    filter_full_capacity_parkings = function(seuil_taux_occupation = 90, nb_heures_par_jour_satures = 3, nb_jour_par_semaine_sature = 2) {

      # calcul pour chaque parking du nombre d'heure / j pdt lequel il est sature
      n_hour_per_day_full <- self$cleaned_data %>%
        copy() %>%
        .[, date := as_date(time)] %>%
        .[, .(n_hour_per_day_full = sum(taux_occupation >= seuil_taux_occupation)), by = list(ident, date)]


      # calcul pour chaque parking du nombre de jours par semaine pendant lequel il est sature au moins `nb_heures_par_jour_satures` heures
      n_days_per_week_full <- n_hour_per_day_full %>%
        copy() %>%
        .[, week := floor_date(date, unit = "week", week_start = 1)] %>%
        .[, .(n_day_per_week_full = sum(n_hour_per_day_full >= nb_heures_par_jour_satures)), by = list(ident, week)]

      # on converse les parkings qui remplissent le critere de saturation journaliere au moins `nb_jour_par_semaine_sature` jours dans une semaine
      self$parkings_satures <- n_days_per_week_full[n_day_per_week_full >= nb_jour_par_semaine_sature]
    },

    #' @description
    #' Realise une calendar heatmap des parkings les plus satures
    #' @param selected_parking parkings à afficher
    #' @param app_theme theme de l'application (dark ou light)
    #' @import data.table
    #' @importFrom ggplot2 ggplot ggtitle aes geom_tile scale_fill_distiller scale_y_continuous scale_x_date facet_wrap theme_minimal theme unit element_blank coord_equal element_text
    #' @importFrom ggiraph geom_tile_interactive
    #' @importFrom glue glue_data
    #' @import ggiraph
    #' @import ggplot2
    #' @importFrom bdxmetroidentity theme_bdxmetro

    calendar_heatmap = function(selected_parking, app_theme) {
      self$data_plot <- self$cleaned_data %>%
        copy() %>%
        .[ident %in% unique(self$parkings_satures$ident)] %>%
        .[, `:=`(hours = lubridate::hour(time), date = as_date(time))] %>%
        .[, tooltip := glue_data(.SD, "Date : {as.character(time)}\nTaux : {sprintf('%.2f', taux_occupation)}")]

      gg <- self$data_plot[ident %in% selected_parking] %>%
        ggplot(., aes(y = date, x = hours, tooltip = tooltip)) +
        geom_tile_interactive(aes(fill = taux_occupation), colour = "white") +
        scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(0, 100)) +
        scale_x_continuous(breaks = 0:23) +
        scale_y_date(date_labels = "%d/%m", breaks = "3 days", expand = c(0, 0)) +
        theme_bdxmetro(app_theme, axis_text_size = 15, axis_title_size = 15) +
        theme(
          legend.position = "bottom",
          text = element_text(size = 16),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        xlab("Heure")

      gg
    }
  )
)


# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
