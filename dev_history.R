usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("renv_setup.R")
usethis::use_build_ignore("deploy_to_RSPM.R")
usethis::use_build_ignore("vignettes/occupationparkingsapp")
usethis::use_build_ignore("vignettes/questions")
usethis::use_build_ignore("vignettes/designApp")
usethis::use_build_ignore("R/_disable_autoload.R")


usethis::use_data_raw("parkings")

usethis::use_vignette("occupationparkingsapp")
usethis::use_vignette("questions")
usethis::use_vignette("designApp")


usethis::use_r("_disable_autoload")

attachment::att_amend_desc()
vignette <- FALSE
devtools::check(document = TRUE, vignettes = vignette)
devtools::build(vignettes = vignette)
devtools::install(build_vignettes = vignette)

usethis::use_test("occupation_compute_xtradata_request_parameters")


pkgload::load_all()

golem::sanity_check()

golem::run_dev()

prefixer::show_nonascii_file()


