
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SC4: Improving Precision and Power in Randomized Trials by Leveraging Baseline Variables

- [Michael Rosenblum](https://mrosenblumbiostat.wordpress.com/), Johns
  Hopkins University - [GitHub](https://github.com/mrosenblum)
- [Kelly Van Lancker](https://kellyvanlancker.com/), Ghent University -
  [GitHub](https://github.com/kelvlanc)
- [Joshua Betz](https://publichealth.jhu.edu/faculty/2679/joshua-betz),
  Johns Hopkins University - [GitHub](https://github.com/jbetz-jhu)

In May 2023, the U.S. Food and Drug Administration (FDA) released
guidance for industry on “Adjustment for Covariates in Randomized
Clinical Trials for Drugs and Biological Products.” Covariate adjustment
is a statistical analysis method for improving precision and power in
clinical trials by adjusting for pre-specified, prognostic baseline
variables. Here, the term “covariates” refers to baseline variables,
that is, variables that are measured before randomization such as age,
gender, BMI, comorbidities. The resulting sample size reductions can
lead to substantial cost savings and more ethical trials since they
avoid exposing more participants than necessary to experimental
treatments. Though covariate adjustment is recommended by the FDA and
the European Medicines Agency (EMA), many trials do not exploit the
available information in baseline variables or only make use of the
baseline measurement of the outcome.

In Part 1, we introduce the concept of covariate adjustment. We explain
what covariate adjustment is, how it works, when it may be useful to
apply, and how to implement it in a preplanned way that is robust to
model misspecification.

In Part 2, we present statistical methods that enable investigators to
easily combine covariate adjustment with trial designs that allow for
interim stopping for efficacy and futility, including information
monitoring and group sequential designs. The result will be faster, more
efficient trials for many disease areas, without sacrificing validity or
power. This approach can lead to faster trials even when the
experimental treatment is ineffective; this may be more ethical in
settings where it is desirable to stop as early as possible to avoid
unnecessary exposure to side effects.

In Part 3, we demonstrate how to implement covariate adjustment across
the life cycle of a study using data and code, including planning new
studies, monitoring ongoing studies, and performing pre-specified
analyses.

Statistical/Programming Knowledge Required: Participants should have a
basic understanding of randomized trials, regression models, and
survival analysis. Familiarity with R is helpful but not required.

## Code Examples

Example R code and data are included in this repository: R can be
installed from the [Comprehensive R Archival Network
(CRAN)](https://cran.r-project.org/). The [R
Studio](https://rstudio.com/) development environment will make it
easier to take full advantage of R. Make sure
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is installed:
this will facilitate compiling R packages from source. After these steps
have been installed, additional packages will need to be installed from
CRAN and GitHub.

``` r
# NOTE: INSTALL Rtools First: https://cran.r-project.org/bin/windows/Rtools/

installed_packages <- installed.packages()[, "Package"]

cran_packages <-
  c("cobalt", "devtools", "digest", "dplyr", "ggplot2", "ggsci", "here",
    "kableExtra", "labelled", "rpact", "stringr", "table1", "tidyr", "xfun")

github_packages <-
  c("jbetz-jhu/impart")

packages_to_install <-
  setdiff(
    x = cran_packages,
    y = installed_packages
  )

if(length(packages_to_install)){
  install.packages(packages_to_install)
}

github_packages_to_install <-
  setdiff(
    x = 
      gsub(
        pattern = "^[A-Za-z0-9_\\.\\-]*/",
        replacement = "",
        x = github_packages
      ),
    y = installed_packages
  )

if(length(github_packages_to_install) > 0){
  for(i in github_packages_to_install){
    devtools::install_github(repo = i, force = TRUE)
  }
}
```
