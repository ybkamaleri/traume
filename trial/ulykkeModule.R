
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
      box(width = 4,
          title = "Type ulykke",
          status = "primary",
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
                                       choices = list("Alle" = 50,
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
    ))
}

ulykkeMod <- function(input, output, session, data){

  ## MISC Objekter
  #########################
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


  ## Reactive Value to return
  vars <- reactiveValues()

  ## Valg relevante kolonner
  valgData <- data[, valgCol, with = FALSE]

  ## Velge ulykketype == TRUE eller transport type == FALSE
  #########################################################
  observe({
    vars$velge  <- ifelse(req(input$ulykke) == 1, FALSE, TRUE)
  })

  ## Liste over ulykke typer
  navnUT <- c("acc_transport",
              "acc_fall",
              "acc_violence",
              "acc_self_inflict",
              "acc_work",
              "acc_sprt_recreat",
              "acc_fire_inhal",
              "acc_other")

  ## Ulykke type kolonne ie. velger alle ulykke type kolonner
  ## eller input$ulykke verdi
  ############################################################
  ulykkeCol <- reactive({
    if(input$ulykke != 9){
      switch(as.numeric(input$ulykke),
             "acc_transport",
             "acc_fall",
             "acc_violence",
             "acc_self_inflict",
             "acc_work",
             "acc_sprt_recreat",
             "acc_fire_inhal",
             "acc_other")
    } else { navnUT }
  })

  ## observe({
  ##   vars$ulykke <- ulykkeCol()
  ## })


  ## Transport typer for var acc_trsp_rd_type
  ############################################
  transVar <- "acc_trsp_rd_type"
  ## alle <- c(1:7, 99, 999)
  transValg <- reactive({
    if(input$transport == 50){
      var <-  c(1:7, 99, 999)
    } else {
      var <- as.numeric(input$transport)
    }
    var
  })

  ## prob1001
  ### OBS!!! #### hvordan velges flere kolonner med verdi 1? Se Test miscR folder under loopDT.R
  ## Filter data - hvis alle transport så velge col 1:9
  ###############
  filtertData <- reactive({
    if(vars$velge == TRUE){

      valgData[, valgVar := {v1 <- unlist(.SD) #ungroup .SDcols
        indUT <- which(v1 == 1)[1] #plukke index som oppfylle kravet
        list(v1[indUT], names(.SD)[indUT])}, #legge verdien på .SDcols ift. index indUT
        .SDcols = ulykkeCol(), by = 1:nrow(valgData)][is.na(valgVar), valgVar := 0]

    } else {

      valgData[(transVar) %in% transValg(), valgVar := 1]

      ## valgData[, valgVar := {v1 <- unlist(.SD)
      ##   indUT <- which(v1 %in% as.numeric(vars$trans))[1]
      ##   list(v1[indUT], names(.SD)[indUT])},
      ##   .SDcols = transVar, by = 1:nrow(valgData)][is.na(valgVar), valgVar := 0]

    }
  })

  ## Filtert data
  observe({
    vars$data  <- filtertData()[valgVar == 1]
  })

  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

    ulykkeCol()
  })

  output$test2 <- renderPrint({

    vars$trans
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
