#' R6 Class pour donnees occupations des parkings
#'
#' @description
#' va gérer la routine API / clean / plot / table / download
Occupation <- R6::R6Class(
  "Occupation",
  inherit = ParkingsStats,
  public = list(
    #' @field aggregated_data_by_some_time_unit Donnees sur lesquelles on applique une fonction d'aggregation par unité de temps
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
    #' @param aggregation_unit aggregation_unit
    #' @param plageHoraire plageHoraire
    #' @param parkings_list liste des parkings analyses
    #' @return A new `Occupation` object.
    
    initialize = function(rangeStart = NULL, rangeEnd = NULL, rangeStep = NULL, aggregation_unit = NULL, plageHoraire = NULL, parkings_list = NULL) {
      super$initialize(rangeStart, rangeEnd, rangeStep, aggregation_unit, plageHoraire, parkings_list)
    },
    
    #' @description
    #' Aggregation des données selon une fenetre temporelle
    #' (application de la fonction summarise_by_time de timetk)
    #' @param time_unit pas d'aggregation à appliquer
    #' @param ... parametres additionels de floor_date
    #' @import data.table
    #' @importFrom lubridate floor_date
    
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
    #' @param aggregation_unit pas de temps pour l'axe des x (heure, jour, wday, mois)
    #' @param app_theme theme de l'application (dark ou light)
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual scale_size_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data glue
    #' @import data.table
    #' @importFrom bdxmetroidentity theme_bdxmetro scale_color_bdxmetro_discrete
    #'
    
    timeseries_plot_1_period = function(parkings_to_plot, aggregation_unit, app_theme) {
      self$data_plot_1_period <- self$aggregated_data_by_some_time_unit %>%
        copy() %>%
        .[ident %in% c(parkings_to_plot, "moyenne")] %>%
        .[, tooltip := as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf(\'%.2f\', taux_occupation)}")
        )] %>%
        .[, linetype := fifelse(ident == "moyenne", "dotted", "solid")] %>%
        .[, lwd := fifelse(ident == "moyenne", 1.5, 1)]  %>% 
        .[ident == "moyenne", nom := paste(nom, "secteur")] #%>% 
      # .[ident == "moyenne", ident := paste(ident, "secteur")]
      
      xlab <- switch(aggregation_unit,
                     "hour" = "Heure",
                     "day" = "Jour")
      
      date_labels_format <- switch(aggregation_unit,
                                   "hour" = "%R",
                                   "day" = "%a %d")
      
      legend_name <- "Occupation parking"
      
      
      gg <- self$data_plot_1_period %>%
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group = nom, linetype = nom, size = nom)) +
        geom_line_interactive(aes(data_id = ident)) +
        geom_point_interactive(aes(tooltip = tooltip, data_id = ident)) +
        scale_x_datetime(date_labels = date_labels_format) +
        theme_bdxmetro(app_theme) +
        scale_linetype_manual(
          values =
            unlist(
              with(
                unique(self$data_plot_1_period[ident %in% c("moyenne", parkings_to_plot), c("nom", "linetype")]),
                split(linetype, nom)
              )
            )
        ) +
        scale_size_manual(
          values =
            unlist(
              with(
                unique(self$data_plot_1_period[ident %in% c("moyenne", parkings_to_plot), c("nom", "lwd")]),
                split(lwd, nom)
              )
            )
        ) +
        xlab(xlab) +
        ylab("Taux d\'occupation (%)") +
        labs(color = legend_name,
             size = legend_name,
             linetype = legend_name,
             scale = legend_name,
             caption = glue("Période étudiée : {min(self$data_plot_1_period$time)} - {max(self$data_plot_1_period$time)}")) +
        scale_color_bdxmetro_discrete() +
        theme(plot.caption = element_text(face="bold.italic", hjust = 0))
      
      gg
    },
    
    #' @description
    #' Graphe de série temporelle avec comparaison de 2 périodes
    #' @param data_occupation_1 donnees d'occupation de la période 1
    #' @param data_occupation_2 donnees d'occupation de la période 2
    #' @param aggregation_unit pas de temps pour l'axe des x (heure, jour, wday, mois)
    #' @param parkings_to_plot liste des parkings à afficher (parametre input shiny)
    #' @param app_theme theme de l'application (dark ou light)
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual scale_size_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data
    #' @import data.table
    #' @importFrom bdxmetroidentity theme_bdxmetro scale_color_bdxmetro_discrete create_palette_bdxmetro
    #'
    
    timeseries_plot_2_periods = function(data_occupation_1, data_occupation_2, aggregation_unit, parkings_to_plot, app_theme) {
      self$data_plot_2_periods <-
        rbind(
          # on rajoute le suffixe _periode1 ou _periode2 pour distinguer les 2 dans la legende du graphe
          data_occupation_1$aggregated_data_by_some_time_unit %>%
            copy() %>%
            .[ident == "moyenne", nom := paste(nom, "secteur_periode1")] %>% 
            .[ident != "moyenne", nom := paste0(nom, "_periode1")],
          data_occupation_2$aggregated_data_by_some_time_unit %>%
            copy() %>%
            .[ident == "moyenne", nom := paste(nom, "secteur_periode2")] %>% 
            .[ident != "moyenne", nom := paste0(nom, "_periode2")]
        ) %>%
        .[, tooltip := as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nTaux : {sprintf(\'%.2f\', taux_occupation)}")
        )] %>%
        .[ident %in% c(parkings_to_plot, "moyenne")] %>%
        .[, linetype := fifelse(ident == "moyenne", "dotted", "solid")] %>%
        .[, lwd := fifelse(ident == "moyenne", 1.5, 1)]
      
      # on va appliquer un format pour la date en fonction de l'unité de temps à appliquer (jour, semaine, mois, annee)
      # pour pouvoir aligner les 2 graphiques sur un axe des x identiques
      # ex si données journalières du 15/07 et du 25/07, ggplot ne peut pas les aligner en fonction de l'heure par défaut
      if (aggregation_unit == "hour") {
        
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := strftime(time, "%H:%M")]
        
        xlab <- "Heure"
        
      } else if (aggregation_unit == "day"  & length(unique(as.Date(self$data_plot_2_periods$time))) %in% 7:14) {
        
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := factor(lubridate::wday(time, label = TRUE, week_start = 1))]
        
        xlab <- "Jour de la semaine"
        
      } else if (aggregation_unit == "day") {
        
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := factor(lubridate::day(time))]
        
        xlab <- "Jour du mois"
        
      } else {
        
        self$data_plot_2_periods <- self$data_plot_2_periods %>%
          .[, time := factor(lubridate::month(time, label = TRUE, abbr = FALSE))]
        
        xlab <- "Mois"
      }
      
      mypal <- create_palette_bdxmetro("discrete")(length(unique(self$data_plot_2_periods$nom)))
      names(mypal) <- sort(unique(self$data_plot_2_periods$nom))
      
      legend_name <- "Occupation parking"
      
      
      gg <- self$data_plot_2_periods %>%
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group = nom, linetype = nom, size = nom)) +
        geom_line_interactive(aes(data_id = nom)) +
        geom_point_interactive(aes(tooltip = tooltip, data_id = nom)) +
        theme_bdxmetro(app_theme) +
        scale_linetype_manual(
          values =
            unlist(
              with(
                unique(self$data_plot_2_periods[ident %in% c("moyenne", parkings_to_plot), c("nom", "linetype")]),
                split(linetype, nom)
              )
            )
        ) +
        scale_size_manual(
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
        ylab("Taux d\'occupation (%)") +
        labs(color = legend_name,
             size = legend_name,
             linetype = legend_name,
             scale = legend_name,
             caption = glue("Période 1 : {min(data_occupation_1$aggregated_data_by_some_time_unit$time)} - {max(data_occupation_1$aggregated_data_by_some_time_unit$time)}
                              Période 2 : {min(data_occupation_2$aggregated_data_by_some_time_unit$time)} - {max(data_occupation_2$aggregated_data_by_some_time_unit$time)}")
        ) +
        theme(plot.caption = element_text(face="bold.italic", hjust = 0))
      
      gg
      
    }
  )
)
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
