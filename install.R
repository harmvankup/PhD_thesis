install.packages("xfun")

if (!require("remotes")) 
  install.packages("remotes", repos = "https://cran.rstudio.org")
remotes::install_github('rstudio/bookdown')


remotes::install_github("ismayc/thesisdown")

install.packages("bookdown")

install.packages(c('tinytex', 'rmarkdown'))
tinytex::install_tinytex()
install.packages("pkgbuild")

remotes::install_github("ismayc/thesisdown", force = TRUE)
remotes::install_github("crsh/citr")
