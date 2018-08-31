
## test ulykke module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")


## MISC Objekter
#########################
## Liste av variablenavn i ullyke datasett som skal plukkes ut
valgVar <- c("acc_transport",
             "acc_fall",
             "acc_violence",
             "acc_self_inflict", #selvpåført
             "acc_work",
             "acc_sprt_recreat", #sport og fritid
             "acc_fire_inhal",
             "acc_other",
             "acc_trsp_rd_type", #transport typer
             "ntrid")


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

  ## Reactive value to return
  ## valg <- reactiveValues()

  ## Valg bort urelevante kolonner
  valgData <- ulykke[, valgVar, with = FALSE, key = ntrid]

  ## Convert code to numeric
  for (i in valgVar){
    set(valgData, j = i, value = as.numeric(valgData[[i]]))
  }


  ## ## Velger kolonne ved input$ulykke
  ## valgCol <-  switch(as.numeric(input$ulykke),
  ##                    "acc_transport",
  ##                    "acc_fall",
  ##                    "acc_violence",
  ##                    "acc_self_inflict",
  ##                    "acc_work",
  ##                    "acc_sprt_recreat",
  ##                    "acc_fire_inhal",
  ##                    "acc_other")

  ## Column name for Type transport
  varTrans <- "acc_trsp_rd_type"

  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

  })

  output$test2 <- renderPrint({
    ## sk <- dim(skadeData)
    ## ul <- dim(ulykkeData)
    ## all <- dim(mainData)
    ## ba <- mainData[!duplicated(ntrid), .N]

    ## paste0("skade: ", sk,
    ##        " ulykke: ", ul,
    ##        " master: ", all,
    ##        " unique: ", ba)


  })


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
