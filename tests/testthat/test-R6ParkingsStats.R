library(testthat)

test_that("R6 Class Occupation Works", {
  
  parc_relais = Occupation$new(rangeStart = "2021-05-10",
                               rangeEnd = "2021-05-11",
                               rangeStep = "hour",
                               timeStep = "hour",
                               plageHoraire = 0:23,
                               localisation_parking = NA,
                               parc_relais = TRUE)
  
  expect_s3_class(parc_relais, "R6")
  expect_s3_class(parc_relais, "Occupation")
  
  parc_relais$download_data(rangeStep = parc_relais$rangeStep)
  expect_equal(dim(parc_relais$data_xtradata), c(672,7))
  
  parc_relais$clean_output()
  expect_equal(as.character(mean(parc_relais$cleaned_data$taux_occupation)), "17.1379039312608")
  
  parc_relais$mean_by_some_time_unit(parc_relais$rangeStep)
  expect_equal(as.character(parc_relais$aggregated_data_by_some_time_unit[1, "taux_occupation"]),  "5.67917661608256")
  expect_equal(as.character(parc_relais$aggregated_data_by_some_time_unit[100, "taux_occupation"]),  "3.87323943661971")
  
  
  gg <- parc_relais$timeseries_plot_1_period(parkings_to_plot = c("CUBPK97"), app_theme = "light")
  
  expect_s3_class(gg, "ggplot")
  
})