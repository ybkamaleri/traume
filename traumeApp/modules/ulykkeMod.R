## Module UI
#################
ulykkeUI <- function(id){

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
    )
  )
}


ulykkeSV <- function(input, output, session, valgDT, data){

  ## filtert data for å velge ntrid valgDT henter data fra filterModule. Bruk is.null
  ## hvis ingen data ikke er filtert enda
  listNTR <- reactive({
    if (is.null(valgDT$data)){
      dataIN <- data.table(ntrid = 0)
    } else {
      dataIN <- as.data.table(valgDT$data)
    }

    valgNTR <- dataIN[, list(ntrid)]
    valgNTR
  })

  ## data som skal brukes
  dataMod <- reactive({
    dataDT <- data
    ntrNR <- listNTR()
    dataDT[ntrNR, on = c(ntrid = "ntrid")]
  })

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
  regData <- reactive({

    regDT = dataMod()[!duplicated(ntrid) & !is.na(ntrid), valgCol, with = FALSE]

    ## Legg alle type ulykke - alleUT : alle ulykke typer
    #######################################################
    regDT[, alleUT := {v1 <- unlist(.SD) #ungroup .SDcols
      indUT <- which(v1 == 1)[1] #plukke index som oppfylle kravet
      list(v1[indUT], names(.SD)[indUT])}, #legge verdien på .SDcols ift. index indUT
      .SDcols = navnUT, by = 1:nrow(regDT)]
  })


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
    colValg <- ulykkeCol()
    regData()[get(colValg) == 1, list(valgCol = get(colValg)),
      keyby = ntrid]
  })


  ## Transport data
  ## Filtert data for transport typer
  ## problem å bruker !is.na(ntrid)
  filDataTrans <- eventReactive(input$transport, {
    transVar <- "acc_trsp_rd_type"
    alleTrans <-  c(1:7, 99, 999)

    if (as.numeric(input$transport) == 9){
      data <- regData()[get(transVar) %in% alleTrans, list(valgCol = acc_trsp_rd_type), keyby = ntrid]
    } else {
      data <- regData()[get(transVar) == as.numeric(input$transport), list(valgCol = acc_trsp_rd_type), keyby = ntrid]
    }
    data
  })

  ## reactiveValues dataUT
  dataUT <- reactiveValues()

  observe({
    dataUT$data <- ifelse(as.numeric(input$ulykke) != 1, filDataUlykke(), filDataTrans())
  })

  ## ## TEST
  ## observe({
  ##   dataUT$data <- listNTR()
  ## })

  return(dataUT)

}
