## test AIS module
##################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

aisModUI <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 3,
          title = "Type ulykke",
          selectInput(inputId = ns("ulykke"),
                      label = NULL,
                      choices = list("Transport" = 1,
                                     "Fall" = 2,
                                     "Vold" = 3,
                                     "Selvpåført" = 4,
                                     "Arbeid" = 5,
                                     "Sport og fritid" = 6,
                                     "Brann og inhalasjon" = 7,
                                     "Annen" = 8,
                                     "Alle type ulykker" = 9),
                      selected = 9,
                      width = '90%')),
      box(width = 3,
          title = "Kroppsregion",
          selectInput(inputId = ns("kropp"),
                      label = NULL,
                      choices = list("Head" = 1,
                                     "Face" = 2,
                                     "Neck" = 3,
                                     "Thorax" = 4,
                                     "Abdomen" = 5,
                                     "Upper extremity" = 7,
                                     "Lower extremity" = 8,
                                     "External and other" = 9),
                      selected = 1,
                      multiple = TRUE,
                      width = '99%')),
      box(width = 3,
          title = 'Skadegradering untatt grad 1',
          checkboxGroupInput(inputId = "sgrad",
                             label = NULL,
                             choices = list("2" = 2,
                                            "3" = 3,
                                            "4" = 4,
                                            "5" = 5,
                                            "6" = 6),
                             inline = TRUE,
                             selected = 2
                             ),
          checkboxInput(inputId = ns("grad1"),
                        label = " include skadegrad 1 i analysen"))
    ))
}

aisMod <- function(input, output, session, data){



}



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
      fluidRow(
        aisModUI("ais")
      ))
  )
)


server <- function(input, output, session){
  callModule(aisMod, "ais")

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
