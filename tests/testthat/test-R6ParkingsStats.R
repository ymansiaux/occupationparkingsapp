library(testthat)

parkings_list <- c("CUBPK44", "CUBPK94", "CUBPK84", "CUBPK97")
tolerance <- 0.00001

test_that("R6 Class Occupation Works", {
  parc_relais <- Occupation$new(
    rangeStart = "2021-05-10",
    rangeEnd = "2021-05-11",
    rangeStep = "hour",
    timeStep = "Jour",
    plageHoraire = 0:23,
    parkings_list = parkings_list
  )

  expect_s3_class(parc_relais, "R6")
  expect_s3_class(parc_relais, "Occupation")

  parc_relais$data_xtradata <- parc_relais$download_data(
    rangeStart = parc_relais$rangeStart,
    rangeEnd = parc_relais$rangeEnd,
    rangeStep = parc_relais$rangeStep,
    plageHoraire = 0:23,
    parkings_list = parkings_list
  )

  expect_equal(dim(parc_relais$data_xtradata), c(96, 8))

  parc_relais$clean_output()
  expect_equal(mean(parc_relais$cleaned_data$taux_occupation), 31.04926, tolerance = tolerance)

  parc_relais$mean_by_some_time_unit(parc_relais$rangeStep)
  expect_equal(parc_relais$aggregated_data_by_some_time_unit[1][["taux_occupation"]], 25, tolerance = tolerance)
  expect_equal(parc_relais$aggregated_data_by_some_time_unit[10][["taux_occupation"]], 37.7137, tolerance = tolerance)

  gg <- parc_relais$timeseries_plot_1_period(parkings_to_plot = parkings_list, timeStep = "hour", app_theme = "light")

  expect_s3_class(gg, "ggplot")
})


test_that("R6 Class Saturation Works", {
  parc_relais <- Saturation$new(
    rangeStart = "2021-06-28",
    rangeEnd = "2021-07-05",
    rangeStep = "hour",
    timeStep = "Semaine",
    plageHoraire = 0:23,
    parkings_list = parkings_list
  )

  expect_s3_class(parc_relais, "R6")
  expect_s3_class(parc_relais, "Saturation")

  parc_relais$data_xtradata <- parc_relais$download_data(
    rangeStart = parc_relais$rangeStart,
    rangeEnd = parc_relais$rangeEnd,
    rangeStep = parc_relais$rangeStep,
    plageHoraire = 0:23,
    parkings_list = parkings_list
  )

  expect_equal(dim(parc_relais$data_xtradata), c(672, 8))

  parc_relais$clean_output()
  expect_equal(mean(parc_relais$cleaned_data$taux_occupation), 47.51924, tolerance = tolerance)

  parc_relais$filter_full_capacity_parkings(seuil_taux_occupation = .8, nb_heures_par_jour_satures = 4, nb_jour_par_semaine_sature = 1)
  expect_equal(dim(parc_relais$parkings_satures), c(4, 3))
  expect_equal(sum(parc_relais$parkings_satures$n_day_per_week_full), 28)


  gg <- parc_relais$calendar_heatmap(selected_parking = parkings_list, app_theme = "light")

  expect_s3_class(gg, "ggplot")
})
