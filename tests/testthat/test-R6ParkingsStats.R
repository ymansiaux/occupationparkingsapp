library(testthat)
library(shiny)
library(xtradata)

# parkings_list <- c("CUBPK44", "CUBPK94", "CUBPK84", "CUBPK97")
tolerance <- 0.00001

## Occupation

server <- function(input, output, session) {
  sample_parc_relais <- (c("CUBPK44", "CUBPK94", "CUBPK84", "CUBPK97"))
  
  parkings_list <- reactive(parkings[,c("ident","nom")])
  
  parc_relais <- Occupation$new(
    rangeStart = "2021-05-10",
    rangeEnd = "2021-05-11",
    rangeStep = "hour",
    timeStep = "Jour",
    plageHoraire = 0:23,
    parkings_list = sample_parc_relais
  )
  
  parc_relais$data_xtradata <- parc_relais$download_data(
    rangeStart = parc_relais$rangeStart,
    rangeEnd = parc_relais$rangeEnd,
    rangeStep = parc_relais$rangeStep,
    plageHoraire = 0:23,
    parkings_list = sample_parc_relais
  )
  print(parkings_list())
  parc_relais$clean_output(parkings_list)
  parc_relais$mean_by_some_time_unit(parc_relais$rangeStep)
  
  gg <- parc_relais$timeseries_plot_1_period(parkings_to_plot = sample_parc_relais, timeStep = "hour", app_theme = "light")
  
}

test_that("R6 Class Occupation Works", {
  
  testServer(server, {
    
    expect_s3_class(parc_relais, "R6")
    expect_s3_class(parc_relais, "Occupation")
    
    expect_equal(dim(parc_relais$data_xtradata), c(96, 8))
    
    expect_equal(mean(parc_relais$cleaned_data$taux_occupation), 8.065677, tolerance = tolerance)
    
    expect_equal(parc_relais$aggregated_data_by_some_time_unit[1][["taux_occupation"]], 0, tolerance = tolerance)
    expect_equal(parc_relais$aggregated_data_by_some_time_unit[10][["taux_occupation"]], 16.9516, tolerance = tolerance)
    
    expect_s3_class(gg, "ggplot")
  })
})


## Saturation

server <- function(input, output, session) {
  sample_parc_relais <- (c("CUBPK44", "CUBPK94", "CUBPK84", "CUBPK97"))
  
  parkings_list <- reactive(parkings[,c("ident","nom")])
  
  parc_relais <- Saturation$new(
    rangeStart = "2021-06-28",
    rangeEnd = "2021-07-05",
    rangeStep = "hour",
    timeStep = "Semaine",
    plageHoraire = 0:23,
    parkings_list = sample_parc_relais
  )
  
  parc_relais$data_xtradata <- parc_relais$download_data(
    rangeStart = parc_relais$rangeStart,
    rangeEnd = parc_relais$rangeEnd,
    rangeStep = parc_relais$rangeStep,
    plageHoraire = 0:23,
    parkings_list = sample_parc_relais
  )
  
  print(parkings_list())
  parc_relais$clean_output(parkings_list)

    parc_relais$filter_full_capacity_parkings(seuil_taux_occupation = .8, nb_heures_par_jour_satures = 4, nb_jour_par_semaine_sature = 1)
  
  gg <- parc_relais$calendar_heatmap(selected_parking = sample_parc_relais, app_theme = "light")
  
  
}


test_that("R6 Class Saturation Works", {
  
  testServer(server, {
    expect_s3_class(parc_relais, "R6")
    expect_s3_class(parc_relais, "Saturation")
    
    expect_equal(dim(parc_relais$data_xtradata), c(672, 8))
    
    expect_equal(mean(parc_relais$cleaned_data$taux_occupation), 30.02566, tolerance = tolerance)
    
    expect_equal(dim(parc_relais$parkings_satures), c(3, 3))
    expect_equal(sum(parc_relais$parkings_satures$n_day_per_week_full), 21)
    
    expect_s3_class(gg, "ggplot")
  })
})
