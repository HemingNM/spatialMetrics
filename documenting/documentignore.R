# install.packages("devtools")
library(devtools)
# devtools::use_testthat()
# devtools::use_vignette("introduction")
## Wickham 2015 R packages (pg 98) Documenting package
# ?devtools::use_package()

### First run
# https://happygitwithr.com/rstudio-git-github.html
# https://community.rstudio.com/t/procedure-to-create-a-project-for-a-package-destined-for-github/2054/4
usethis::create_package(getwd())
# devtools::create("nbThin")
# devtools::build(getwd())

### Configuring github
usethis::git_sitrep()
usethis::git_vaccinate()
credentials::set_github_pat("7354617e689df6ee2e5a39ca4a5f92b221c15ac4")

usethis::use_build_ignore(c("documenting", "parallel.md"))
devtools::document()
devtools::check()
# devtools::load_all()
# roxygen2::roxygenise()

devtools::install_github("HemingNM/spatialMetrics")

# for building windows package
devtools::build_win()
path <- "../ENMwizard_src"
if(dir.exists(path)==F){dir.create(path)}
devtools::build(path=path)




# # general function documentation
#
# #' Function Title (short description)
# #'
# #' General function description. A short paragraph (or more) describing what the function does.
# #' @param arg1 List of occurence data. See argument "occ_locs" in mxnt.cp.
# #' @inheritParams function1
# #' @return objects returned from function
# #' @examples
# #' plot(mxnt.mdls.preds.lst[[1]][[4]]) # MaxEnt predictions, based on the model selection criteria
# #' @export
#


# Bvarieg.occ <- read.table(paste(system.file(package="dismo"), "/ex/bradypus.csv", sep=""), header=TRUE, sep=",")
# colnames(Bvarieg.occ) <- c("SPEC", "LONG", "LAT")
# spp.occ.list <- list(Bvarieg = Bvarieg.occ)
# occ_polys <- f.poly.batch(spp.occ.list, o.path="occ_poly")
