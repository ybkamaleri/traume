
## test ulykke module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

source("~/Git-work/traume/ntrApp/data.R")

## Module UI
#################
ModuleUI <- function(id){

  ns  <- NS(id)
  ## input er character eller string type derfor må konverteres til tall
  fluidPage(
    fluidRow(
      box(width = 4,
          title = "Type ulykke",
          selectInput(inputId = ns("ulykke"),
                      label = NULL,
                      choices = list("Alle" = 9,
                                     "Transport" = 1,
                                     "Fall" = 2,
                                     "Vold" = 3,
                                     "Selvpåført" = 4,
                                     "Arbeid" = 5,
                                     "Sport og fritid" = 6,
                                     "Brann og inhalasjon" = 7,
                                     "Annen" = 8
                                     ),
                      selected = 9
                      ),
          conditionalPanel(condition = paste0("input['", ns("ulykke"), "'] == 1"),
                           selectInput(inputId = ns("transport"),
                                       label = "Valg transport type:",
                                       choices = list("Alle" = 9,
                                                      "Bil" = 1,
                                                      "MC" = 2,
                                                      "Sykkel" = 3,
                                                      "Båt" = 4,
                                                      "Tog" = 5,
                                                      "Fly" = 6,
                                                      "Moped" = 7,
                                                      "Annet" = 99,
                                                      "Ukjent" = 999
                                                      ),
                                       selected = 50
                                       )))
    ),
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


ModuleServer <- function(input, output, session, data){

  ## Liste av variablenavn i ullyke datasett som skal plukkes ut
  valgCol <- c("acc_transport",
               "acc_fall",
               "acc_violence",
               "acc_self_inflict", #selvpåført
               "acc_work",
               "acc_sprt_recreat", #sport og fritid
               "acc_fire_inhal",
               "acc_other",
               "acc_trsp_rd_type", #transport typer
               "ntrid")

  ## Liste over ulykke typer
  navnUT <- c("acc_transport",
              "acc_fall",
              "acc_violence",
              "acc_self_inflict",
              "acc_work",
              "acc_sprt_recreat",
              "acc_fire_inhal",
              "acc_other")

  ## Valg relevant kolonner og bort med duplicated id og NA
  regData <- data[!duplicated(ntrid) & !is.na(ntrid), valgCol, with = FALSE]

  ## Legg alle type ulykke - alleUT : alle ulykke typer
  #######################################################
  regData[, alleUT := {v1 <- unlist(.SD) #ungroup .SDcols
    indUT <- which(v1 == 1)[1] #plukke index som oppfylle kravet
    list(v1[indUT], names(.SD)[indUT])}, #legge verdien på .SDcols ift. index indUT
    .SDcols = navnUT, by = 1:nrow(regData)]

  ## Valg kolonne
  ulykkeCol <- reactive({
    switch(as.numeric(input$ulykke),
           "acc_transport",
           "acc_fall",
           "acc_violence",
           "acc_self_inflict",
           "acc_work",
           "acc_sprt_recreat",
           "acc_fire_inhal",
           "acc_other",
           "alleUT")
  })


  ## Filtert data for ulykke typer
  filDataUlykke <- eventReactive(input$ulykke, {
    regData[get(ulykkeCol()) == 1, list(valgCol = get(ulykkeCol())),
            keyby = ntrid]
  })


  ## Transport data
  ## Transport typer for var acc_trsp_rd_type
  ############################################
  ## transport type "IKKE VALGT" kodet 0 ikke brukes
  transVar <- "acc_trsp_rd_type"

  transValg <- reactive({
    if(as.numeric(input$transport) == 9){
      var <-  c(1:7, 99, 999)
    } else {
      var <- as.numeric(input$transport)
    }
    var
  })

  ## Filtert data for transport typer
  ## problem å bruker !is.na(ntrid)
  filDataTrans <- eventReactive(input$transport, {
    regData[get(transVar) %in% transValg(), list(valgCol = transValg()),
            keyby = ntrid]
  })


  ## Reactive Value to return
  ############################
  vars <- reactiveValues()

  ## Velge ulykketype == TRUE eller transport type == FALSE
  #########################################################
  ## observe({
  ##   vars$velge  <- ifelse(as.numeric(input$ulykke) == 1, 1, 2)
  ## })

  observe({
    vars$dataUT <- ifelse(input$ulykke != 1, filDataUlykke(), filDataTrans())
  })


  ## ## reactiveVal style
  ## dataUT <- reactiveVal()

  ## observe({
  ##   dataUT(ifelse(input$ulykke != 1, filDataUlykke(), filDataTrans()))
  ## })



  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({
    str(vars$dataUT)
  })

  output$test2 <- renderPrint({
    transValg()
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
          ModuleUI("ulykke")
        ))
    )
  )


  server <- function(input, output, session){
    callModule(ModuleServer, "ulykke", ulykke)

    session$onSessionEnded(stopApp)
  }

  shinyApp(ui, server)
