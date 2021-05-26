usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("deploy_to_RSPM.R")

usethis::use_data_raw("parkings")

devtools::check(document = TRUE)
devtools::build(vignettes = TRUE)
devtools::install(build_vignettes = TRUE)

pkgload::load_all()


