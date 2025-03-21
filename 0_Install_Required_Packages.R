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