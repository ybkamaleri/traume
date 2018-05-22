library(shiny)

#create a function to check for installed packages and install them if they are not installed
install.packages <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# usage
req.packages <- c("shinydashboard", "DT", "data.table")
install.packages(req.packages)


## useMe <- c("shiny", "shinydashboard", "DT", "data.table")
## lapply(useMe, require)
source('data.R', local = TRUE)
