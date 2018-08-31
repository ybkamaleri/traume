
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
  valgKolom <- c("acc_transport",
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
  valgData <- data[, valgKolom, with = FALSE]

  ## Velge ulykke eller transport kolonne
  #######################################
  observe({
    vars$velge  <- ifelse(req(input$ulykke) == 1, FALSE, TRUE)
  })

  ## Ulykke type kolonne
  ########################
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
    }else{
      c("acc_transport",
        "acc_fall",
        "acc_violence",
        "acc_self_inflict",
        "acc_work",
        "acc_sprt_recreat",
        "acc_fire_inhal",
        "acc_other")
    }
  })

  observe({
    vars$ulykke <- ulykkeCol()
  })


  ## Transport typer for var acc_trsp_rd_type
  ############################################
  alle <- c(1:7, 99, 999)
  observe({
    vars$trans <- ifelse(req(input$transport) == 50,
                         paste(alle, collapse = ","),
                         input$transport)
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
