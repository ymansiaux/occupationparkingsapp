usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("deploy_to_RSPM.R")

usethis::use_data_raw("parkings")

usethis::use_vignette("occupationparkingsapp")
usethis::use_vignette("questions")

usethis::use_r("_disable_autoload")

devtools::check(document = TRUE)
devtools::build(vignettes = TRUE)
devtools::install(build_vignettes = TRUE)

pkgload::load_all()

golem::sanity_check()

golem::run_dev()
