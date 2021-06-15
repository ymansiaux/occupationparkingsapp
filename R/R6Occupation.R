#' R6 Class pour donnees occupations des parkings
#'
#' @description
#' va gérer la routine API / clean / plot / table / download
Occupation <- R6::R6Class(
  "Occupation",
  inherit = ParkingsStats,
  
  public = list(
    
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
      self$data_xtradata <- 
        # on bind_rows : la moyenne globale et la moyenne par ident
        bind_rows.(
          self$data_xtradata %>% 
            mutate.(time = floor_date(time, unit = time_unit, ...)) %>% 
            summarise.(taux_occupation = mean(taux_occupation, na.rm = TRUE), .by = c(time)) %>% 
            mutate.(ident = "moyenne")
          ,
          
          self$data_xtradata %>% 
            mutate.(time = floor_date(time, unit = time_unit, ...)) %>% 
            summarise.(taux_occupation = mean(taux_occupation, na.rm = TRUE), .by = c(ident, time))
        )
    },
    
    #' @description
    #' Graphe de série temporelle
    #' @param parkings_to_plot liste des parkings à afficher (parametre input shiny)
    #' @param show_average affichage de la moyenne (boolean)
    #' @param horaires horaires d'interet
    #' @importFrom ggplot2 ggplot aes geom_line scale_linetype_manual theme_minimal theme scale_color_manual
    #' @importFrom ggiraph geom_line_interactive geom_point_interactive
    #' @importFrom glue glue_data
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom lubridate hour
    #'  
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"), show_average = TRUE)
    #' } 
    timeseries_plot = function(parkings_to_plot, show_average = TRUE) {
      
      data_plot <-  self$data_xtradata %>% 
        # filter.(hour(time) %in% horaires) %>% 
        mutate.(tooltip = as.character(
          glue_data(.SD, "Date : {as.character(time)}\nnom : {nom}\nVal : {sprintf('%.2f', taux_occupation)}")
        )) %>% 
        mutate.(linetype = ifelse(ident == "moyenne", "dotted", "solid"))
      
      
      gg <- filter.(data_plot, ident %in% parkings_to_plot & ident != "moyenne") %>%  
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = nom, group=nom, linetype = nom)) + 
        geom_line_interactive(aes(data_id=ident), lwd = 1) + 
        geom_point_interactive(aes(tooltip=tooltip, data_id=ident)) + 
        theme_minimal() +
        theme(legend.position = "bottom") +
        
        geom_line_interactive(data = data_plot %>% filter.(ident == "moyenne"), 
                              mapping = aes(x = time, y = taux_occupation, tooltip=taux_occupation, data_id = ident, group = nom, color = nom),
                              lwd = 1.5) +
        scale_linetype_manual(
          "nom",
          values =
            unlist(
              with(
                distinct.(data_plot %>% 
                            filter.(ident %in% c("moyenne", parkings_to_plot)) %>%
                            select.(nom, linetype)),
                split(linetype, nom)))
        ) +
        # A modifier quand on aura la palette bx metro
        scale_color_manual(values = sample(colors(distinct = TRUE), length(parkings_to_plot)+1))
      

      gg
      
    }
  )
  
)
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html