usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("deploy_to_RSPM.R")

devtools::check(document = TRUE)
devtools::build(vignettes = TRUE)
devtools::install(build_vignettes = TRUE)

pkgload::load_all()