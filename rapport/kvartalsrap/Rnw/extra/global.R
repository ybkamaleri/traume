
### pakker ###
pakke <- c("shiny",
  "shinyBS",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "plotly",
  "dygraphs",
  "knitr",
  "kableExtra",
  "xts",
  "zoo",
  "DT")

## sapply(pakke, require, character.only = TRUE)

load <- function(pkg){
  nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nypkg))
    install.packages(nypkg, dependencies = TRUE, repos = "http://cran.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}

load(pakke)

#Funksjoner
############
invisible(sapply(list.files('functions', full.names = TRUE), source))
