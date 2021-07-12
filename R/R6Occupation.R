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
    #' @param data_xtradata data_xtradata
    #' @return A new `Occupation` object.
    
    initialize = function(rangeStart, rangeEnd, rangeStep, timeStep, plageHoraire, localisation_parking, parc_relais, aggregated_data_by_some_time_unit) {
      super$initialize(rangeStart, rangeEnd, rangeStep, timeStep, plageHoraire, localisation_parking, parc_relais)
    },
    
    #' @description
    #' Aggregation des données selon une fenetre temporelle
    #' (application de la fonction summarise_by_time de timetk)
    #' @param time_unit pas d'aggreg (voir timetk)
    #' @param ... parametres additionels de floor_date
    #' @import data.table
    #' @importFrom lubridate floor_date
    #' @examples \dontrun{ temporal_aggregate("day")
    #' } 
    mean_by_some_time_unit = function(time_unit, ...) {
       # browser()
      self$aggregated_data_by_some_time_unit <-
        
        rbind(
          self$cleaned_data %>% 
            copy() %>% 
            .[, time := floor_date(time, unit = time_unit, ...)] %>% 
            .[, .(taux_occupation = mean(taux_occupation, na.rm = TRUE)), by = time] %>% 
            .[, `:=`(ident = "moyenne", nom = "moyenne")]
          ,
          self$cleaned_data %>% 
            copy() %>% 
            .[, time := floor_date(time, unit = time_unit, ...)] %>% 
            .[, .(taux_occupation = mean(taux_occupation, na.rm = TRUE)), by = list(ident, nom, time)]
        )
    },
    
    
    #' @description
    #' Graphe de série temporelle
    #' @param parkings_to_plot liste des parkings à afficher (parametre input shiny)
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data
    #' @import data.table
    #' @importFrom lubridate hour
    #'  
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"))
    #' } 
    timeseries_plot_1_period = function(parkings_to_plot) {
      
      self$data_plot_1_period <-  self$aggregated_data_by_some_time_unit %>% 
        copy() %>% 
        .[ident %in% c(parkings_to_plot, "moyenne")] %>% 
        .[, tooltip := as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf('%.2f', taux_occupation)}")
        )] %>% 
        .[, linetype := fifelse(ident == "moyenne", "dotted", "solid")]
      
      gg <- self$data_plot_1_period[ident %in% parkings_to_plot & ident != "moyenne"] %>%  
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group=nom, linetype = nom)) + 
        geom_line_interactive(aes(data_id=ident), lwd = 1) + 
        geom_point_interactive(aes(tooltip=tooltip, data_id=ident)) + 
        theme_minimal() +
        theme(legend.position = "right") +
        
        geom_line_interactive(data = self$data_plot_1_period[ident == "moyenne"], 
                              mapping = aes(x = time, y = taux_occupation, tooltip = tooltip, data_id = ident, group = nom, color = nom),
                              lwd = 1.5) +
        scale_linetype_manual(
          "Parking",
          values =
            unlist(
              with(
                unique(self$data_plot_1_period[ident %in% c("moyenne", parkings_to_plot), c("nom", "linetype")]),
                split(linetype, nom)))
        ) + 
        xlab("Heure") +
        ylab("Taux d'occupation (%)") +
        labs(color = "Parking", scale = "Parking") +
        # A modifier quand on aura la palette bx metro
        scale_color_manual(values = sample(colors(distinct = TRUE), length(parkings_to_plot)+1))
      gg
    },
    
    #' @description
    #' Graphe de série temporelle avec comparaison de 2 périodes
    #' @param data_occupation_1 données d'occupation de la période 1
    #' @param data_occupation_2 données d'occupation de la période 2
    #' @param timeStep pas de temps pour l'axe des x (heure, jour, wday, mois)
    #' @param parkings_to_plot liste des parkings à afficher (parametre input shiny)
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data
    #' @import data.table
    #' @importFrom lubridate hour wday day month
    #'  
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"), show_average = TRUE)
    #' } 
    timeseries_plot_2_periods = function(data_occupation_1, data_occupation_2, timeStep, parkings_to_plot) {
      
      self$data_plot_2_periods <- data_occupation_1$aggregated_data_by_some_time_unit %>% 
        copy() %>% 
        .[,nom := paste0(nom, "_periode1")] %>% 
        rbind(., 
              data_occupation_2$aggregated_data_by_some_time_unit %>% 
                copy() %>% 
              .[, nom := paste0(nom, "_periode2")]
        ) %>% 
        .[,tooltip := as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nTaux : {sprintf('%.2f', taux_occupation)}")
        )] %>% 
        .[,linetype := fifelse(ident == "moyenne", "dotted", "solid")]  %>% 
        .[ident %in% c(parkings_to_plot, "moyenne")]
      
      if(timeStep == "Jour") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          .[, time := strftime(time, "%H:%M")]
        
        xlab <- "Heure"
        
      } else if(timeStep == "Semaine") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          .[, time := factor(wday(time, label = TRUE, week_start = 1))]
        
        xlab <- "Jour de la semaine"
        
      } else if(timeStep == "Mois") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          .[, time := factor(day(time))]
        
        xlab <- "Jour du mois"
        
      } else {
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          .[, time := factor(month(time, label = TRUE, abbr = FALSE))]
        
        xlab <- "Mois"
      }
      
      gg <- self$data_plot_2_periods[ident %in% parkings_to_plot & ident != "moyenne"] %>%  
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group=nom, linetype = nom)) + 
        geom_line_interactive(aes(data_id=ident), lwd = 1) + 
        geom_point_interactive(aes(tooltip=tooltip, data_id=ident))  +
        theme_minimal() +
        
        geom_line_interactive(data = self$data_plot_2_periods[ident == "moyenne"], 
                              mapping = aes(x = time, y = taux_occupation, tooltip = tooltip, data_id = ident, group = nom, color = nom),
                              lwd = 1.5) +
        scale_linetype_manual(
          "Parking",
          values =
            unlist(
              with(
                unique(self$data_plot_2_periods[ident %in% c("moyenne", parkings_to_plot), c("nom", "linetype")]),
                split(linetype, nom)))
        )  +
        xlab(xlab) +
        ylab("Taux d'occupation (%)") +
        labs(color = "Parking", scale = "Parking")
      
      gg
      
    }
    
    
  )
  
)
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
