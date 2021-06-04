#' R6 Class pour donnees occupations des parkings
#'
#' @description
#' va gérer la routine API / clean / plot / table / download
Occupation <- R6::R6Class(
  "Occupation",
  inherit = Parkings,
  
  public = list(
    
    #' @description
    #' Aggregation des données selon une fenetre temporelle
    #' (application de la fonction summarise_by_time de timetk)
    #' @param time_unit pas d'aggreg (voir timetk)
    #' @import tidytable
    #' @importFrom data.table :=
    #' @importFrom timetk summarise_by_time
    #' @importFrom dplyr bind_rows group_by
    #' @examples \dontrun{ temporal_aggregate("day")
    #' } 
    mean_by_some_time_unit = function(time_unit) {
      self$data_xtradata <- 
        # on bind_rows : la moyenne globale et la moyenne par ident
        bind_rows(
          self$data_xtradata %>% 
            summarise_by_time(.date_var = time, 
                              .by = time_unit,
                              taux_occupation = mean(taux_occupation, na.rm = TRUE)) %>% 
            mutate.(ident = "moyenne")
          ,
          
          self$data_xtradata %>% 
            group_by(ident) %>% 
            summarise_by_time(.date_var = time, 
                              .by = time_unit,
                              taux_occupation = mean(taux_occupation, na.rm = TRUE))
        )
    },
    
    #' @description
    #' Graphe de série temporelle
    #' @importFrom ggplot2 ggplot aes geom_line
    #' @examples \dontrun{ timeseries_plot()
    #' } 
    timeseries_plot = function() {
      ggplot(data = self$data_xtradata, mapping = aes(x = time, y = taux_occupation, color = ident)) + 
        geom_line()
    }
  )
  
)
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html