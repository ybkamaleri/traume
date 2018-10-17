
## test ulykke module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

## Module UI
#################
skadeModUI <- function(id){

  ns  <- NS(id)

  fluidPage(
    fluidRow(
      verbatimTextOutput(ns("test"))
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    ),
    fluidRow(
    )
  )
}


skadeMod <- function(input, output, session, data){




  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

  })

  output$test2 <- renderPrint({


  })

}


###################################
########## Shiny App ##############
###################################

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
