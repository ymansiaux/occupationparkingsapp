test_that("Test du jour suivant OK", {
  
  day <- "2021-05-11"
  
  res <- occupation_compute_xtradata_request_parameters("Jour", day)
  
  expect_true(all(sapply(res, class) == c("Date", "Date", "character")))
  expect_equal(res$rangeEnd, as.Date("2021-05-12"))
  
})


test_that("Test de la semaine suivante OK", {
  
  day <- "2021-05-11"
  
  res <- occupation_compute_xtradata_request_parameters("Semaine", day)
  
  expect_true(all(sapply(res, class) == c("Date", "Date", "character")))
  expect_equal(res$rangeStart, as.Date("2021-05-10"))
  expect_equal(res$rangeEnd, as.Date("2021-05-16"))
  
})


test_that("Test du mois suivant OK", {
  
  day <- "2021-05-11"
  
  res <- occupation_compute_xtradata_request_parameters("Mois", day)
  
  expect_true(all(sapply(res, class) == c("Date", "Date", "character")))
  expect_equal(res$rangeStart, as.Date("2021-05-01"))
  expect_equal(res$rangeEnd, as.Date("2021-05-31"))
  
})


test_that("Test de l'année suivante OK", {
  
  day <- "2021"
  
  res <- occupation_compute_xtradata_request_parameters("Année", day)
  
  expect_true(all(sapply(res, class) == c("Date", "Date", "character")))
  expect_equal(res$rangeStart, as.Date("2021-12-31"))
  
  if(lubridate::year(Sys.Date()) == day)  expect_equal(res$rangeEnd, Sys.Date())
  if(lubridate::year(Sys.Date()) != day)  expect_equal(res$rangeEnd, "2021-12-31")
  
  
})

