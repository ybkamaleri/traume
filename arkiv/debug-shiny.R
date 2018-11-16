## Brukers for å kjøre debuging fordi C-c s funker ikke fro debuging
library(shiny)
appDir <- getwd()
runApp(appDir)


## Testing
runApp(appDir, display.mode = "showcase")

## Hvis ikke allerede installert
library(devtools)
install_github("rstudio/shinytest")

## snapshot
library(shinytest)
recordTest(appDir)

## test
testApp(appDir, "mytest")
