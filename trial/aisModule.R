## test AIS module
##################

library(shiny)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

aisModUI <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 3,
          selectInput(inputId = ns("ulykke"),
                      label = NULL,
                      choices = list("Transport" = 1,
                                     "Fall" = 2,
                                     "Vold" = 3,
                                     "Selvpåført" = 4,
                                     "Arbeid" = 5,
                                     "Sport og fritid" = 6,
                                     "Brann og inhalasjon" = 7,
                                     "Annen" = 8),
                      selected = 1,
                      width = '90%'))))
}

aisMod <- function(input, output, session, data){

}




ui <- fluidPage(
  aisModUI("ais")
)

server <- function(input, output, session){
  callModule(aisMod, "ais")

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
