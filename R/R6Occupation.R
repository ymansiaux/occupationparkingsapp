#' R6 Class pour donnees occupations des parkings
#'
#' @description
#' va gérer la routine API / clean / plot / table / download
Occupation <- R6::R6Class(
  "Occupation",
  inherit = ParkingsStats,
  public = list(
    #' @field aggregated_data_by_some_time_unit Données sur lesquelles on applique une fonction d'aggregation par unité de temps
    aggregated_data_by_some_time_unit = NULL,
    
    #' @field data_plot_1_period donnée du graphique pour 1 seule période étudiée
    data_plot_1_period = NULL,
    
    #' @field data_plot_2_periods donnée du graphique pour 2 période étudiées
    data_plot_2_periods = NULL,
    
    #' @description
    #' Create a new occupation object.
    #' @param rangeStart rangeStart
    #' @param rangeEnd rangeEnd
    #' @param rangeStep rangeStep
    #' @param timeStep timeStep
    #' @param plageHoraire plageHoraire
    #' @param localisation_parking localisation_parking
    #' @param parc_relais parc_relais
    #' @return A new `Occupation` object.
    
    initialize = function(rangeStart = NULL, rangeEnd = NULL, rangeStep = NULL, timeStep = NULL, plageHoraire = NULL, localisation_parking, parc_relais) {
      super$initialize(rangeStart, rangeEnd, rangeStep, timeStep, plageHoraire, localisation_parking, parc_relais)
    },
    
    #' @description
    #' Aggregation des données selon une fenetre temporelle
    #' (application de la fonction summarise_by_time de timetk)
    #' @param time_unit pas d'aggregation à appliquer
    #' @param ... parametres additionels de floor_date
    #' @import data.table
    #' @importFrom lubridate floor_date
    #' @examples \dontrun{ temporal_aggregate("day")
    #' }
    mean_by_some_time_unit = function(time_unit, ...) {
      self$aggregated_data_by_some_time_unit <-
        rbind(
          self$cleaned_data %>%
            copy() %>%
            .[, time := floor_date(time, unit = time_unit, ...)] %>%
            .[, .(taux_occupation = mean(taux_occupation, na.rm = TRUE)), by = time] %>%
            .[, `:=`(ident = "moyenne", nom = "moyenne")],
          self$cleaned_data %>%
            copy() %>%
            .[, time := floor_date(time, unit = time_unit, ...)] %>%
            .[, .(taux_occupation = mean(taux_occupation, na.rm = TRUE)), by = list(ident, nom, time)]
        )
    },
    
    
    #' @description
    #' Graphe de série temporelle
    #' @param parkings_to_plot liste des parkings à afficher (parametre input shiny)
    #' @param app_theme theme de l'application (dark ou light)
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual scale_size_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data
    #' @import data.table
    #' @importFrom bdxmetroidentity theme_bdxmetro scale_color_bdxmetro_discrete
    #'
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"))
    #' }
    timeseries_plot_1_period = function(parkings_to_plot, timeStep, app_theme) {
      self$data_plot_1_period <- self$aggregated_data_by_some_time_unit %>%
        copy() %>%
        .[ident %in% c(parkings_to_plot, "moyenne")] %>%
        .[, tooltip := as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf('%.2f', taux_occupation)}")
        )] %>%
        .[, linetype := fifelse(ident == "moyenne", "dotted", "solid")] %>% 
        .[, lwd := fifelse(ident == "moyenne", 1.5, 1)]
      
      xlab <- switch(timeStep,
                     "Jour" = "Heure",
                     "Semaine" = "Jour",
                     "Mois" = "Jour",
                     "Année" = "Mois"
      )
      
      gg <- self$data_plot_1_period %>%
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group = nom, linetype = nom, size = nom)) +
        geom_line_interactive(aes(data_id = ident)) +
        geom_point_interactive(aes(tooltip = tooltip, data_id = ident)) +
        theme_bdxmetro(app_theme) +
        scale_linetype_manual(
          "Parking",
          values =
            unlist(
              with(
                unique(self$data_plot_1_period[ident %in% c("moyenne", parkings_to_plot), c("nom", "linetype")]),
                split(linetype, nom)
              )
            )
        ) +
        scale_size_manual(
          "Parking",
          values =
            unlist(
              with(
                unique(self$data_plot_1_period[ident %in% c("moyenne", parkings_to_plot), c("nom", "lwd")]),
                split(lwd, nom)
              )
            )
        ) +
        xlab(xlab) +
        ylab("Taux d'occupation (%)") +
        labs(color = "Parking", size = "Parking", scale = "Parking") +
        scale_color_bdxmetro_discrete()
      
      gg
      
      
    },
    
    #' @description
    #' Graphe de série temporelle avec comparaison de 2 périodes
    #' @param data_occupation_1 données d'occupation de la période 1
    #' @param data_occupation_2 données d'occupation de la période 2
    #' @param timeStep pas de temps pour l'axe des x (heure, jour, wday, mois)
    #' @param parkings_to_plot liste des parkings à afficher (parametre input shiny)
    #' @param app_theme theme de l'application (dark ou light)
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual scale_size_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data
    #' @import data.table
    #' @importFrom bdxmetroidentity theme_bdxmetro scale_color_bdxmetro_discrete create_palette_bdxmetro
    #'
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"), show_average = TRUE)
    #' }
    timeseries_plot_2_periods = function(data_occupation_1, data_occupation_2, timeStep, parkings_to_plot, app_theme) {
      self$data_plot_2_periods <-
        rbind(
          # on rajoute le suffixe _periode1 ou _periode2 pour distinguer les 2 dans la legende du graphe
          data_occupation_1$aggregated_data_by_some_time_unit %>%
            copy() %>%
            .[, nom := paste0(nom, "_periode1")],
          data_occupation_2$aggregated_data_by_some_time_unit %>%
            copy() %>%
            .[, nom := paste0(nom, "_periode2")]
        ) %>%
        .[, tooltip := as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nTaux : {sprintf('%.2f', taux_occupation)}")
        )] %>%
        .[ident %in% c(parkings_to_plot, "moyenne")] %>% 
        .[, linetype := fifelse(ident == "moyenne", "dotted", "solid")] %>%
        .[, lwd := fifelse(ident == "moyenne", 1.5, 1)]
      
      
      # on va appliquer un format pour la date en fonction de l'unité de temps à appliquer (jour, semaine, mois, annee)
      # pour pouvoir aligner les 2 graphiques sur un axe des x identiques
      # ex si données journalières du 15/07 et du 25/07, ggplot ne peut pas les aligner en fonction de l'heure par défaut
      if (timeStep == "Jour") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := strftime(time, "%H:%M")]
        
        xlab <- "Heure"
      } else if (timeStep == "Semaine") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := factor(lubridate::wday(time, label = TRUE, week_start = 1))]
        
        xlab <- "Jour de la semaine"
      } else if (timeStep == "Mois") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := factor(day(time))]
        
        xlab <- "Jour du mois"
      } else {
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := factor(lubridate::month(time, label = TRUE, abbr = FALSE))]
        
        xlab <- "Mois"
      }
      
      # if(length(parkings_to_plot) >3) browser()
      
      mypal <- create_palette_bdxmetro("discrete")(length(unique(self$data_plot_2_periods$nom)))
      names(mypal) <- unique(self$data_plot_2_periods$nom)
      
      gg <- self$data_plot_2_periods %>%
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group = nom, linetype = nom, size = nom)) +
        geom_line_interactive(aes(data_id = nom)) +
        geom_point_interactive(aes(tooltip = tooltip, data_id = nom)) +
        theme_bdxmetro(app_theme) +
        scale_linetype_manual(
          "Parking",
          values =
            unlist(
              with(
                unique(self$data_plot_2_periods[ident %in% c("moyenne", parkings_to_plot), c("nom", "linetype")]),
                split(linetype, nom)
              )
            )
        ) +
        scale_size_manual(
          "Parking",
          values =
            unlist(
              with(
                unique(self$data_plot_2_periods[ident %in% c("moyenne", parkings_to_plot), c("nom", "lwd")]),
                split(lwd, nom)
              )
            )
        ) +
        scale_color_manual(
          values = mypal
        ) +
        xlab(xlab) +
        ylab("Taux d'occupation (%)") +
        labs(color = "Parking", size = "Parking", scale = "Parking")
      
      gg
      
    }
  )
)
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
