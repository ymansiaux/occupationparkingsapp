library(testthat)

test_that("R6 Class Occupation Works", {
  parc_relais <- Occupation$new(
    rangeStart = "2021-05-10",
    rangeEnd = "2021-05-11",
    rangeStep = "hour",
    timeStep = "Jour",
    plageHoraire = 0:23,
    localisation_parking = NA,
    parc_relais = TRUE
  )
  
  expect_s3_class(parc_relais, "R6")
  expect_s3_class(parc_relais, "Occupation")
  
  parc_relais$data_xtradata <- parc_relais$download_data(
    rangeStart = parc_relais$rangeStart, 
    rangeEnd = parc_relais$rangeEnd, 
    rangeStep = parc_relais$rangeStep,
    plageHoraire = 0:23,
    localisation_parking = NA,
    parc_relais = TRUE)
  
  expect_equal(dim(parc_relais$data_xtradata), c(672, 7))
  
  parc_relais$clean_output()
  expect_equal(as.character(mean(parc_relais$cleaned_data$taux_occupation)), "17.1379039312608")
  
  parc_relais$mean_by_some_time_unit(parc_relais$rangeStep)
  expect_equal(as.character(parc_relais$aggregated_data_by_some_time_unit[1, "taux_occupation"]), "5.67917661608256")
  expect_equal(as.character(parc_relais$aggregated_data_by_some_time_unit[100, "taux_occupation"]), "3.87323943661971")
  
  
  gg <- parc_relais$timeseries_plot_1_period(parkings_to_plot = c("CUBPK97"), timeStep = "hour", app_theme = "light")
  
  expect_s3_class(gg, "ggplot")
})


test_that("R6 Class Saturation Works", {
  parc_relais <- Saturation$new(
    rangeStart = "2021-06-28",
    rangeEnd = "2021-07-05",
    rangeStep = "hour",
    timeStep = "Semaine",
    plageHoraire = 0:23,
    localisation_parking = NA,
    parc_relais = TRUE
  )
  
  expect_s3_class(parc_relais, "R6")
  expect_s3_class(parc_relais, "Saturation")
  
  parc_relais$data_xtradata <- parc_relais$download_data(
    rangeStart = parc_relais$rangeStart, 
    rangeEnd = parc_relais$rangeEnd, 
    rangeStep = parc_relais$rangeStep,
    plageHoraire = 0:23,
    localisation_parking = NA,
    parc_relais = TRUE)
  
  expect_equal(dim(parc_relais$data_xtradata), c(4704, 7))
  
  parc_relais$clean_output()
  expect_equal(as.character(mean(parc_relais$cleaned_data$taux_occupation)), "21.2941925772493")
  
  parc_relais$filter_full_capacity_parkings(seuil_taux_occupation = .8, nb_heures_par_jour_satures = 4, nb_jour_par_semaine_sature = 1)
  expect_equal(dim(parc_relais$parkings_satures), c(28, 3))
  expect_equal(sum(parc_relais$parkings_satures$n_day_per_week_full), 186)
  
  
  gg <- parc_relais$calendar_heatmap(selected_parking = c("CUBPK98"), app_theme = "light")
  
  expect_s3_class(gg, "ggplot")
})
