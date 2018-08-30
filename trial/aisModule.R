
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
                                     "Alle" = 9),
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
                                                      "Alle" = 50),
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
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    )
  )
}


## Liste av variablenavn i ullyke datasett som skal plukkes ut
varUlykkeType <- c("ntrid",
                   "acc_transport",
                   "acc_trsp_rd_type", #transport typer
                   "acc_fall",
                   "acc_violence",
                   "acc_self_inflict",
                   "acc_work",
                   "acc_other",
                   "acc_sprt_recreat", #sport og fritid
                   "acc_fire_inhal")


aisMod <- function(input, output, session, data, skade, ulykke, minNTR, maxNTR){

  ## ##Reactive input
  ## ais <- reactiveValues()

  ## observe(
  ##   ais$data <- data()[, list(ntrid, Hospital, HF, RHF, age, gender)]
  ## )
  ## return(ais)


  valgData <- data[, list(ntrid, Hospital, HF, RHF, age, gender), key = .(ntrid)]

  ## select only ais and ntr skade fil
  skadeData <- skade[ntrid %in% minNTR:maxNTR, list(ntrid, ais), key = .(ntrid)]
  ## Merge alle koder fra samme ntrid
  skadeData[skadeData[, toString(unlist(strsplit(ais, split = ","))),
                      by = ntrid], on = "ntrid", aiskode := i.V1]

  ## select ulykketype variabler fra ulykke data
  ulykkeData <- ulykke[ntrid %in% minNTR:maxNTR, varUlykkeType,
                       with = FALSE, key = .(ntrid)]
  ## Convert code to numeric
  for (i in varUlykkeType){
    set(ulykkeData, j = i, value = as.numeric(ulykkeData[[i]]))
  }

  ## Merge skade og ulykke data
  mergeData <- ulykkeData[skadeData, on = "ntrid"]

  ## Merge med subset data fra menyen
  rowMainData <- valgData[mergeData, on = "ntrid"]
  mainData <- rowMainData[!duplicated(ntrid)]



  ### First filter Data for ulykketype
  ###################################
  ## Hvis "Alle" er valg så velges hele data

  ulykkeFData  <- reactive({

    ## Valg ulykketype annen enn alle
    if (as.numeric(input$ulykke) != 9){

      kode <-  switch(as.numeric(input$ulykke),
                      "acc_transport",
                      "acc_fall",
                      "acc_violence",
                      "acc_self_inflict",
                      "acc_work",
                      "acc_sprt_recreat",
                      "acc_fire_inhal",
                      "acc_other")

      mainData[get(kode) == 1]
    } else {
      ## Alle type ulykke
      ## as.numeric(input$ulykke)
      mainData
    }
  })


  ## Second filter for transport
  ###############################
  transFData <- reactive({

    varValg <- "acc_trsp_rd_type"

    if (as.numeric(input$transport) == 50){
      kode <- c(1:7, 99, 999)
      ulykkeFData()[get(varValg) %in% kode]
    } else {
      kode <- as.numeric(input$transport)
      ulykkeFData()[get(varValg) %in% kode]
    }
  })


  ## Kroppsregion
  ###############




  ### Data






  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({
    ## setkey(skadeData, ntrid)
    ## skadeData[duplicated(ntrid) | duplicated(ntrid, fromLast = TRUE)]
    ## as.numeric(input$kropp)
    str(transFData())
  })

  output$test2 <- renderPrint({
    sk <- dim(skadeData)
    ul <- dim(ulykkeData)
    all <- dim(mainData)
    ba <- mainData[!duplicated(ntrid), .N]

    paste0("skade: ", sk,
           " ulykke: ", ul,
           " master: ", all,
           " unique: ", ba)

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
  callModule(aisMod, "ais", masterFile, skade, ulykke, 600, 4000)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
