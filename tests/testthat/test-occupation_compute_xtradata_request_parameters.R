library(testthat)
# a <- xtradata_requete_aggregate(key = Sys.getenv("XTRADATA_KEY"), typename = "ST_PARK_P", rangeStart = res$rangeStart, rangeEnd = res$rangeEnd, rangeStep = res$rangeStep, filter = list("ident"="CUBPK88"))
# tail(sort(a$time))

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
  expect_equal(res$rangeEnd, as.Date("2021-05-17"))
})


test_that("Test du mois suivant OK", {
  day <- "2021-05-11"

  res <- occupation_compute_xtradata_request_parameters("Mois", day)

  expect_true(all(sapply(res, class) == c("Date", "Date", "character")))
  expect_equal(res$rangeStart, as.Date("2021-05-01"))
  expect_equal(res$rangeEnd, as.Date("2021-06-01"))
})


test_that("Test de l'année suivante OK", {
  day <- "2020"

  res <- occupation_compute_xtradata_request_parameters("Année", day)

  expect_true(all(sapply(res, class) == c("Date", "Date", "character")))
  expect_equal(res$rangeStart, as.Date("2020-04-01"))

  if (lubridate::year(Sys.Date()) == day) expect_equal(res$rangeEnd, Sys.Date())
  if (lubridate::year(Sys.Date()) != day) expect_equal(res$rangeEnd, as.Date("2021-01-01"))
})
