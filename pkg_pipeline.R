library(usethis) # https://usethis.r-lib.org/reference/index.html

#### Infrastructure ####

use_gpl3_license(name = "Oliver Pfaffel") # required to share improvements

use_package("rlang", "Imports")
use_package("ClusterR", "Imports")
use_package("copula", "Imports")
use_package("dplyr", "Imports")
use_package("magrittr", "Imports")
use_package("psych", "Suggests")
use_package("tidyr", "Suggests")
use_package("mclust", "Suggests")
use_package("ggplot2", "Suggests")
use_package("Hmisc", "Suggests")
use_package("tictoc", "Suggests")
use_package("corrplot", "Suggests")
use_pipe() # Use %>%


### create R files
use_r("ClustImpute")
use_r("Missing_simulation")


### Documentation
use_news_md()
use_readme_md()
use_vignette("Example_on_simulated_data")
use_spell_check() # requires spelling package

## Tests
use_testthat()
use_test("MCAR")
use_test("no_NAs")
use_test("only_NAs")
use_test("dimensions")
use_test("prediction")
use_test("var_reduction")
# use_coverage() # Test coverage

## Package website
# use_pkgdown() # https://github.com/r-lib/pkgdown
# pkgdown::build_site()
#  check into git, then go to settings for your repo and make sure that the GitHub pages source is set to “master branch /docs folder”. Be sure to update the URL on your github repository homepage so others can easily navigate to your new site.

## github
use_git() # git remote add origin https://github.com/o1iv3r/ClustImpute.git
# use_github()

## Build ignore --> CRAN notes
use_build_ignore(c("pkg_pipeline.R"))

## Citation
use_citation()

#### Before ANY changes are made ####

# new branch

## Increment version
usethis::use_version() #  increments the "Version" field in DESCRIPTION, adds a new heading to NEWS.md (if it exists), and commits those changes (if package uses Git).

#### Manual deployment ####

## Document and check package
# restart R session first
devtools::test()
devtools::spell_check()
devtools::document()
devtools::check(document = FALSE)

# finally, either release
devtools::release()
# or build and upload manually
devtools::build()

#### CI ####

# optional: add dev branch to travis.yml

use_travis()
use_build_ignore("travis.yml")

use_coverage()
use_build_ignore("codecov.yml")


#### Tinytex ####

# Installing latex packages
tinytex::tlmgr_install("pdfpages")
