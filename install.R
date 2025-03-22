install.packages("xfun")

if (!require("remotes")) 
  install.packages("remotes", repos = "https://cran.rstudio.org")
remotes::install_github('rstudio/bookdown')


remotes::install_github("ismayc/thesisdown")

install.packages("bookdown")


install.packages(c('tinytex', 'rmarkdown'))
library(tinytex)
library(rmarkdown)
library(knitr)
library(bookdown)
library(thesisdown)
install_tinytex()
tinytex:::install_yihui_pkgs()

reinstall_tinytex(packages = TRUE,force = T)
tinytex_root(error = TRUE)
tinytex::check_installed()
install.packages("pkgbuild")
is_tinytex()
tinytex_root()

remotes::install_github("ismayc/thesisdown", force = TRUE)
remotes::install_github("crsh/citr")

tinytex::parse_packages()
tinytex::tlmgr_update()
update.packages(ask = FALSE, checkBuilt = TRUE)
