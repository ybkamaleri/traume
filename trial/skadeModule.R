
## test ulykke module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

## MISC

## Abdomen Tilleggsuttrekk
exAbdomen <- factor(c( "Leverskader", "Miltskader"))

## Ryggsøyle tillegguttrekk
exRygg <- factor(c("cervicalcolumna","lumbalcolumna", "thoracalcolumna"))



## Module UI
#################
skadeModUI <- function(id){

  ns  <- NS(id)

  fluidPage(

    fluidRow(
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
                       selectInput(inputId = ns("til_abdomen"),
                                   label = "Tilleggsuttrekk:",
                                   choices = exAbdomen)),
      ## Tillegg Ryggsøyle
      conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 6"),
                       selectInput(inputId = ns("til_rygg"),
                                   label = "Tilleggsuttrekk:",
                                   choices = exRygg))
    ),
    fluidRow(
      verbatimTextOutput(ns("test"))
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    )
  )
}


skadeMod <- function(input, output, session, data){

  ## Reactive value
  vars <- reactiveValues()



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
        skadeModUI("skade")
      ))
  )
)


server <- function(input, output, session){
  callModule(skadeMod, "skade", skade)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
