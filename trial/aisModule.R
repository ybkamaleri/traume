## test AIS module
##################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

## Abdomen Tilleggsuttrekk
exAbdomen <- factor(c( "Leverskader", "Miltskader"))

## Ryggsøyle tillegguttrekk
exRygg <- factor(c("cervicalcolumna","lumbalcolumna", "thoracalcolumna"))


aisModUI <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 4,
          title = "Type ulykke",
          status = "primary",
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
                      selected = 9
                      ),
          conditionalPanel(condition = paste0("input['", ns("ulykke"), "'] == 1"),
                           selectInput(inputId = ns("transport"),
                                       label = "Valg transport type:",
                                       choices = list("Bil" = 1,
                                                      "MC" = 2,
                                                      "Sykkel" = 3,
                                                      "Båt" = 4,
                                                      "Tog" = 5,
                                                      "Fly" = 6,
                                                      "Moped" = 7,
                                                      "Annet" = 99,
                                                      "Ukjent" = 999,
                                                      "Alle typer" = 50),
                                       selected = 50
                                       ))),
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
                                     "Alle kroppsregioner" = 10),
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
          ),
      box(width = 4,
          title = 'Skadegradering fra 2-6',
          status = "primary",
          checkboxGroupInput(inputId = ns("skadegrad"),
                             label = NULL,
                             choices = list("2" = 2,
                                            "3" = 3,
                                            "4" = 4,
                                            "5" = 5,
                                            "6" = 6),
                             inline = TRUE,
                             selected = NULL
                             ),
          checkboxInput(inputId = ns("skadegrad1"),
                        label = "include skadegrad 1 i analysen",
                        value = TRUE))
    ),
    fluidRow(
      verbatimTextOutput(ns("test"))
    ))
}

## Variablenavn i data uttrekk
varUlykkeType <- c("acc_transport",
                   "acc_fall",
                   "acc_violence",
                   "acc_self_inflict",
                   "acc_work",
                   "acc_other",
                   "acc_sprt_recreat",
                   "acc_fire_inhal")


aisMod <- function(input, output, session, data, skade, ulykke){

  ## ##Reactive input
  ## ais <- reactiveValues()

  ## observe(
  ##   ais$data <- data()[, list(ntrid, Hospital, HF, RHF, age, gender)]
  ## )
  ## return(ais)

  valgData <- data[, list(ntrid, Hospital, HF, RHF, age, gender)]


  output$test <- renderPrint({

    str(valgData)
  })

}



ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
      fluidRow(
        aisModUI("ais")
      ))
  )
)


server <- function(input, output, session){
  callModule(aisMod, "ais", skade, ulykke, 200, 4000)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
