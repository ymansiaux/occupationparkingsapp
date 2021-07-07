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
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom lubridate floor_date
    #' @examples \dontrun{ temporal_aggregate("day")
    #' } 
    mean_by_some_time_unit = function(time_unit, ...) {
      self$aggregated_data_by_some_time_unit <- 
        # on bind_rows : la moyenne globale et la moyenne par ident
        bind_rows.(
          self$cleaned_data %>% 
            mutate.(time = floor_date(time, unit = time_unit, ...)) %>% 
            summarise.(taux_occupation = mean(taux_occupation, na.rm = TRUE), .by = c(time)) %>% 
            mutate.(ident = "moyenne", nom = "moyenne")
          ,
          
          self$cleaned_data %>% 
            mutate.(time = floor_date(time, unit = time_unit, ...)) %>% 
            summarise.(taux_occupation = mean(taux_occupation, na.rm = TRUE), .by = c(ident, nom, time))
        )
    },
    
    #' @description
    #' Graphe de série temporelle
    #' @param parkings_to_plot liste des parkings à afficher (parametre input shiny)
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom lubridate hour
    #'  
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"))
    #' } 
    timeseries_plot_1_period = function(parkings_to_plot) {
      
      self$data_plot_1_period <-  self$aggregated_data_by_some_time_unit %>% 
        filter.(ident %in% c(parkings_to_plot, "moyenne")) %>% 
        mutate.(tooltip = as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf('%.2f', taux_occupation)}")
        )) %>% 
        mutate.(linetype = ifelse(ident == "moyenne", "dotted", "solid"))
      
      
      gg <- filter.(self$data_plot_1_period, ident %in% parkings_to_plot & ident != "moyenne") %>%  
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group=nom, linetype = nom)) + 
        geom_line_interactive(aes(data_id=ident), lwd = 1) + 
        geom_point_interactive(aes(tooltip=tooltip, data_id=ident)) + 
        theme_minimal() +
        theme(legend.position = "right") +
        
        geom_line_interactive(data = self$data_plot_1_period %>% filter.(ident == "moyenne"), 
                              mapping = aes(x = time, y = taux_occupation, tooltip = tooltip, data_id = ident, group = nom, color = nom),
                              lwd = 1.5) +
        scale_linetype_manual(
          "nom",
          values =
            unlist(
              with(
                distinct.(self$data_plot_1_period %>% 
                            filter.(ident %in% c("moyenne", parkings_to_plot)) %>%
                            select.(nom, linetype)),
                split(linetype, nom)))
        ) +
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
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom lubridate hour wday day month
    #'  
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"), show_average = TRUE)
    #' } 
    timeseries_plot_2_periods = function(data_occupation_1, data_occupation_2, timeStep, parkings_to_plot) {
      
      self$data_plot_2_periods <- data_occupation_1$aggregated_data_by_some_time_unit %>% 
        mutate.(nom = paste0(nom, "_periode1")) %>% 
        bind_rows.(data_occupation_2$aggregated_data_by_some_time_unit %>% 
                     mutate.(nom = paste0(nom, "_periode2"))) %>% 
        mutate.(tooltip = as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf('%.2f', taux_occupation)}")
        )) %>% 
        mutate.(linetype = ifelse(ident == "moyenne", "dotted", "solid"))  %>% 
        filter.(ident %in% c(parkings_to_plot, "moyenne"))
      
      
      if(timeStep == "Jour") {
        # browser()
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          mutate.(time = strftime(time, "%H:%M"))
      } else if(timeStep == "Semaine") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          mutate.(time = factor(wday(time, label = TRUE, week_start = 1)))
      } else if(timeStep == "Mois") {
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          mutate.(time = factor(day(time)))
      } else {
        self$data_plot_2_periods <- self$data_plot_2_periods %>% 
          mutate.(time = factor(month(time, label = TRUE, abbr = FALSE)))
      }
      
      gg <- filter.(self$data_plot_2_periods, ident %in% parkings_to_plot & ident != "moyenne") %>%  
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group=nom, linetype = nom)) + 
        geom_line_interactive(aes(data_id=ident), lwd = 1) + 
        geom_point_interactive(aes(tooltip=tooltip, data_id=ident))  +
        theme_minimal() +
        
        geom_line_interactive(data = self$data_plot_2_periods %>% filter.(ident == "moyenne"), 
                              mapping = aes(x = time, y = taux_occupation, tooltip = tooltip, data_id = ident, group = nom, color = nom),
                              lwd = 1.5) +
        scale_linetype_manual(
          "nom",
          values =
            unlist(
              with(
                distinct.(self$data_plot_2_periods %>% 
                            filter.(ident %in% c("moyenne", parkings_to_plot)) %>%
                            select.(nom, linetype)),
                split(linetype, nom)))
        ) #+
      
      gg
      
    }
    
    
  )
  
)
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
