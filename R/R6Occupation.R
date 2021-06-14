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
    #' @importFrom ggplot2 ggplot aes geom_line scale_color_viridis_d theme_minimal theme
    #' @importFrom ggiraph geom_line_interactive
    #' @import tidytable
    #' @importFrom data.table :=
    #' 
    #' @examples \dontrun{ timeseries_plot(parkings_to_plot = c("A","B"), show_average = TRUE)
    #' } 
    timeseries_plot = function(parkings_to_plot, show_average = TRUE) {
      
      data_plot <-  self$data_xtradata %>% mutate.(tooltip = as.character(glue::glue("Date : {.[,time]}\nIdent : {.[,ident]}\nVal : {.[,taux_occupation]}")))
      
      gg <- filter.(data_plot, ident %in% parkings_to_plot & ident != "moyenne") %>%  
        ggplot(data = ., mapping = aes(x = time, y = taux_occupation, color = ident, group=ident)) + 
        geom_line_interactive(aes(tooltip=tooltip, data_id=ident)) + 
        scale_color_viridis_d() +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      if(show_average) {
        gg <- gg + geom_line_interactive(data = data_plot %>% filter.(ident == "moyenne"), mapping = 
         aes(x = time, y = taux_occupation, tooltip=taux_occupation, data_id = ident),  color = "black", lwd = 1.5, linetype = "dotted") +
          scale_color_viridis_d()
      }
      
   gg

    }
  )
  
)
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html