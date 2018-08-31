
## test ulykke module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

## Module UI
#################
ulykkeModUI <- function(id){

  ns <- NS(id)
  fluidPage(
    fluidRow(
      verbatimTextOutput(ns("test"))
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    ),
    fluidRow(
      box(width = 4,
          title = "Kroppsregion",
          status = "primary",
          selectInput(inputId = ns("kropp"),
                      label = NULL,
                      choices = list("Head" = 1,
                                     "Face" = 2,
                                     "Neck" = 3,
                                     "Thorax" = 4,
                                     "Abdomen" = 5,
                                     "Ryggsøyle" = 6,
                                     "Upper extremity" = 7,
                                     "Lower extremity" = 8,
                                     "External and other" = 9,
                                     "Alle" = 10),
                      selected = 10
                      ),
          ## Tillegg abdomen
          conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 5"),
                           selectInput(inputId = ns("tillegg_abdomen"),
                                       label = "Tilleggsuttrekk:",
                                       choices = exAbdomen)),
          ## Tillegg Ryggsøyle
          conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 6"),
                           selectInput(inputId = ns("tillegg_rygg"),
                                       label = "Tilleggsuttrekk:",
                                       choices = exRygg))

          )
    )
}

  ulykkeMod <- function(input, output, session, data){


    ## Reactive value to return
    vars <- reactiveValues()


    ##################
    ###### TEST ######
    ##################

    output$test <- renderPrint({

    })

    output$test2 <- renderPrint({


    })

    ## Return values
    ## velge - for å velge ulykke type kolonne eller ikke
    ## ulykke - kolonne for ulykke å velge
    ## trans - value for transport
    return(vars)

}


ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
      fluidRow(
        ulykkeModUI("ulykke")
      ))
  )
)


server <- function(input, output, session){
  callModule(ulykkeMod, "ulykke", ulykke)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
