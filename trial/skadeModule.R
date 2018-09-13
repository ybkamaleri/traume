## test skade module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(shinyBS)

## source("~/Git-work/traume/ntrApp/data.R")

## Module UI
#################
skadeModUI <- function(id){

  ns  <- NS(id)

  fluidPage(
    fluidRow(
      box(with = 4,
          title = "Kroppsregioner",
          selectInput(inputId = ns("kropp"),
                      label = NULL,
                      choices = list("Alle" = 10,
                                     "Head" = 1,
                                     "Face" = 2,
                                     "Neck" = 3,
                                     "Thorax" = 4,
                                     "Abdomen" = 5,
                                     "Spine" = 6,
                                     "Upper extremity" = 7,
                                     "Lower extremity" = 8,
                                     "External and other" = 9
                                     ),
                      selected = 10
                      ),
          ## Tillegg abdomen
          conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 5"),
                           selectInput(inputId = ns("til_abdomen"),
                                       label = "Tilleggsuttrekk:",
                                       choices = list("Alle" = 1,
                                                      "Leverskader" = 2,
                                                      "Miltskader" = 3
                                                      ),
                                       selected = 3
                                       )),
          ## Deler for Ryggsøyle
          conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 6"),
                           selectInput(inputId = ns("til_rygg"),
                                       label = "Spesifisert deler:",
                                       choices = list("Alle" = 1,
                                                      "Cervicalcolumna" = 2,
                                                      "Lumbalcolumna" = 3,
                                                      "Thoracalcolumna" = 4)
                                       )),
          ## Tillegg for Cervicalcolumna
          conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                              "'] == 2 && input['", ns("kropp"), "'] == 6"),
                           selectInput(inputId = ns("til_cerv"),
                                       label = "Tilleggsuttrekk (nakke):",
                                       choices = list("Alle" = 1,
                                                      "Isolerte skjelettskader" = 2,
                                                      "Ryggmargsskade" = 3
                                                      ),
                                       selected = 1),
                           bsTooltip(id = ns("til_cerv"),
                                     title = "Telling for minst en av de allerede spesifiserte AIS koder",
                                     placement = "right",
                                     trigger = "hover",
                                     options = list(container = "body"))),
          ## Tillegg for Lumbalcolumna
          conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                              "'] == 3 && input['", ns("kropp"), "'] == 6"),
                           selectInput(inputId = ns("til_lumb"),
                                       label = "Tilleggsuttrekk (korsrygg):",
                                       choices = list("Alle" = 1,
                                                      "Isolerte skjelettskader" = 2,
                                                      "Ryggmargsskade" = 3
                                                      ),
                                       selected = 1),
                           bsTooltip(id = ns("til_lumb"),
                                     title = "Telling for minst en av de allerede spesifiserte AIS koder",
                                     placement = "right",
                                     trigger = "hover",
                                     options = list(container = "body"))),
          ## Tillegg for Thoracalcolumna
          conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                              "'] == 4 && input['", ns("kropp"), "'] == 6"),
                           selectInput(inputId = ns("til_thor"),
                                       label = "Tilleggsuttrekk (thorax):",
                                       choices = list("Alle" = 1,
                                                      "Isolerte skjelettskader" = 2,
                                                      "Ryggmargsskade" = 3
                                                      ),
                                       selected = 1),
                           bsTooltip(id = ns("til_thor"),
                                     title = "Telling for minst en av de allerede spesifiserte AIS koder",
                                     placement = "right",
                                     trigger = "hover",
                                     options = list(container = "body")))),

      box(with = 4,
          title = "Skadegradering",
          status = "primary",
          checkboxGroupInput(inputId = ns("skadegrad"),
                             label = NULL,
                             choices = list("1" = 1,
                                            "2" = 2,
                                            "3" = 3,
                                            "4" = 4,
                                            "5" = 5,
                                            "6" = 6),
                             inline = TRUE,
                             selected = 1
                             ),
          checkboxInput(inputId = ns("skadegrad1"),
                        label = "Andel inkluderer skadegrad 1",
                        value = TRUE),
          bsTooltip(id = ns("skadegrad1"),
                    title = "Hvis skadegrad 1 er valgt må denne også velges",
                    placement = "bottom",
                    trigger = "focus",
                    options = list(container = "body")
                    )
          )
    ),
    fluidRow(
      verbatimTextOutput(ns("test"))
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    )
  )
}


skadeMod <- function(input, output, session, dataFiltert, data){

  ## Data må filtreres med 'dataFiltert' når modulen skal implemeteres

  ## OBS!! bruk 'aisMix' for å velge skade gradering
  dataRaw <- data #her skal det merge med dataFiltert
  dataRaw[, aisMix := toString(unlist(strsplit(ais, split = ","))), by = ntrid]


  dataIN <- dataRaw[!duplicated(ntrid)]
  dataIN[, ais := NULL] #slett ais siden aisMix inneholder alle ais koder

  valKropp <- reactive({as.numeric(input$kropp)}) #kroppregion
  valSkade <- reactive({as.numeric(input$skadegrad)}) #skadegrad


  ## Kroppsregioner
  #########################
  ## Minst en av valgte deler + skadegradering

  valgKropp  <- reactive({

    ## Alle kroppsregioner
    if (as.numeric(input$kropp) == 10) {
      req(input$skadegrad) #vises ingen hvis NULL

      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0(".[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

    } else {

      ## Valgte kroppsregion
      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]
    }

    dataUT <- data[n == 1]
    return(dataUT)
    })

  ## Tilleggsuttrekk Abdomen
  ##########################
  ## Minst en av valgte deler + skadegradering

  tilAbdomen <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    ## Milt og lever først 4 tall
    milt <- 5442
    lever <- 5418

    kodeTillegg <- ifelse(as.numeric(input$til_abdomen) == 2, lever, milt)

    if (as.numeric(input$til_abdomen) == 1){

      data <- valgKropp()

    } else {
      ## tar bort kolonn "n"
      dataRaw <- valgKropp()[, n := NULL]

      data <- dataRaw[, list(n = ifelse(
        sum(grepl(paste0("^", kodeTillegg, ".*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]
    }

    dataUT <- data[n == 1]
    return(dataUT)
  })


  ## Spine deler
  #########################
  ## Minst en av valgte deler + skadegradering
  tilSpine <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 1){

      data <- valgKropp()

    } else if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 2){

      ## tar bort kolonn "n"
      dataRaw <- valgKropp()[, n := NULL]

      ## Cervicalcolumna - femte tallet er 2
      data <- dataRaw[, list(n = ifelse(
        sum(grepl(paste0("6[0-9][0-9][0-9]2.*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

    } else if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 3){

      ## tar bort kolonn "n"
      dataRaw <- valgKropp()[, n := NULL]

      ## Lumbalcolumna - femte tallet er 6
      data <- dataRaw[, list(n = ifelse(
        sum(grepl(paste0("6[0-9][0-9][0-9]6.*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

    } else {

      ## tar bort kolonn "n"
      dataRaw <- valgKropp()[, n := NULL]

      ## Thoracalcolumna - femte tallet er 4
      data <- dataRaw[, list(n = ifelse(
        sum(grepl(paste0("6[0-9][0-9][0-9]4.*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

    }

    dataUT <- data[n == 1]
    return(dataUT)
  })



  #####################################################
  ## VIKTIG - Data kilder må endres med filtret data ##
  #####################################################

  ## Spine Tilleggsuttrekk - Cervicalcolumna
  tilCerv <- eventReactive(input$til_cerv, {

    kode_skjelett <- "^6502[1-3][024678].*[23]$"
    kode_rygg <- "^6402.*[3-6]$"

    ## kode er skjelettskader og kode2 ryggmargsskade
    dataSK <- dataIN[, list(
      kode = ifelse(
        sum(grepl(kode_skjelett,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      kode2 = ifelse(
        sum(grepl(kode_rygg,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      ntrid = ntrid,
      gender = gender,
      age = age), by = ntrid]

    #kode1 0 hvis begge skjelettskader og ryggmargsskade
    dataSK[, kode1 := kode, by = ntrid]
    dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]

    if (as.numeric(input$til_cerv) == 1){
      data <- tilSpine()
    } else if (as.numeric(input$til_cerv) == 2){
        data <- dataSK[kode1 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    } else {
      data <- dataSK[kode2 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    }

    dataUT <- data[n == 1]
    return(dataUT)

  })


  ## Spine tilleggsuttrekk - Lumbalcolumna
  tilLumb <- eventReactive(input$til_lumb, {

    kode_skjelett <- "^6506[1-3][024678].*[23]$"
    kode_rygg <- "^6406.*[3-5]$"

    ## kode er skjelettskader og kode2 ryggmargsskade
    dataSK <- dataIN[, list(
      kode = ifelse(
        sum(grepl(kode_skjelett,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      kode2 = ifelse(
        sum(grepl(kode_rygg,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      ntrid = ntrid,
      gender = gender,
      age = age), by = ntrid]
    
    #kode1 0 hvis begge skjelettskader og ryggmargsskade
    dataSK[, kode1 := kode, by = ntrid]
    dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]
    
    if (as.numeric(input$til_lumb) == 1){
      data <- tilSpine()
    } else if (as.numeric(input$til_lumb) == 2){
        data <- dataSK[kode1 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    } else {
      data <- dataSK[kode2 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    }

    dataUT <- data[n == 1]
    return(dataUT)
  })


  ## Spine tilleggsuttrekk - Thoracalcolumna
  tilThor <- eventReactive(input$til_thor, {

    kode_skjelett <- "^6504[1-3][024678].*[23]$"
    kode_rygg <- "^6404.*[3-5]$"
    
    ## kode er skjelettskader og kode2 ryggmargsskade
    dataSK <- dataIN[, list(
      kode = ifelse(
        sum(grepl(kode_skjelett,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      kode2 = ifelse(
        sum(grepl(kode_rygg,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      ntrid = ntrid,
      gender = gender,
      age = age), by = ntrid]
    
    #kode1 0 hvis begge skjelettskader og ryggmargsskade
    dataSK[, kode1 := kode, by = ntrid]
    dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]
    
    if (as.numeric(input$til_thor) == 1){
      data <- tilSpine()
    } else if (as.numeric(input$til_thor) == 2){
      data <- dataSK[kode1 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    } else {
      data <- dataSK[kode2 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    }

    dataUT <- data[n == 1]
    return(dataUT)
  })


  ## hvis tillegg !=1 så velge output fra tillegg input
  ## ellers velger valgKropp

  ###########
  ## Andel ##
  ###########

  ## Inkluderer Grad 1 eller ikke til å beregne andel
  andelGradAlle  <- reactive({
    if (input$skadegrad1){
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(".*[1-6]$", as.character(unlist(
          strsplit(aisMix, split = ","))))) != 0, 1, 0)), by = ntrid]
    } else {
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(".*[2-6]$", as.character(unlist(
          strsplit(aisMix, split = ","))))) != 0, 1, 0)), by = ntrid]
    }
    andelG[, sum(n, na.rm = TRUE)]
  })

  ## Andell med grad 1 eller ikke for spesifiserte kroppsregion
  andelGradKropp <- reactive({
    if (input$skadegrad1){
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[1-6]$"),
                  as.character(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
        gender = gender, aisMix = aisMix), by = ntrid]
    } else {
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[2-6]$"),
                  as.character(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
        gender = gender, aisMix = aisMix), by = ntrid]
    }

    data <- andelG[, sum(n, na.rm = TRUE)]
    return(data)
  })

  ## Data Ut observe
  ###################

  ## dataUT <- reactiveValues()

  ## observe({
  ##    if (as.numeric(input$kropp) %in% c(1:4, 10)){
  ##      dataUT$data <- valgKropp()
  ##    } else if (as.numeric(input$kropp) == 5 & as.numeric(input$til_abdomen) %in% 1:3){
  ##      dataUT$data <- tilAbdomen()
  ##    } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) %in% 1:4) {
  ##      dataUT$data <- tilSpine()
  ##    } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 2 & as.numeric(input$til_cerv) %in% 1:3){
  ##      dataUT$data <- tilCerv()
  ##    } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 3 & as.numeric(input$til_lumb) %in% 1:3){
  ##      dataUT$data <- tilLumb()
  ##    } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 4 & as.numeric(input$til_thor) %in% 1:3){
  ##      dataUT$data <- tilThor()
  ##    }

  ## })

  ############
  ## Tabell ##
  ############
  tabUT <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    ## progress indikator
    progress <- Progress$new(session, min=1, max=5)
    on.exit(progress$close())

    progress$set(message = 'Vent',
                 detail = 'kalkulering pågår...')

    for (i in 1:5) {
      progress$set(value = i)
      Sys.sleep(0.3)
    }

    ## Valg data
    ## if (as.numeric(input$kropp) %in% c(1:4, 10)){
    ##   dataUT <- valgKropp()
    ## } else if (as.numeric(input$kropp) == 5 & as.numeric(input$til_abdomen) %in% 1:3){
    ##   dataUT <- tilAbdomen()
    ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) %in% 1:4) {
    ##   dataUT <- tilSpine()
    ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 2 & as.numeric(input$til_cerv) %in% 1:3){
    ##   dataUT <- tilCerv()
    ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 3 & as.numeric(input$til_lumb) %in% 1:3){
    ##   dataUT <- tilLumb()
    ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 4 & as.numeric(input$til_thor) %in% 1:3){
    ##   dataUT <- tilThor()
    ## }


    ## Valg data
    if (as.numeric(input$kropp) %in% c(1:4, 10)){
      dataUT <- valgKropp()
    }

    if (as.numeric(input$kropp) == 5 & as.numeric(input$til_abdomen) %in% 1:3){
      dataUT <- tilAbdomen()
    }

    if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) %in% 1:4) {
      dataUT <- tilSpine()
    }

    if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 2 & as.numeric(input$til_cerv) %in% 1:3){
      dataUT <- tilCerv()
    }

    if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 3 & as.numeric(input$til_lumb) %in% 1:3){
      dataUT <- tilLumb()
    }

    if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 4 & as.numeric(input$til_thor) %in% 1:3){
      dataUT <- tilThor()
    }

    return(dataUT)

  })


  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

    valgKropp()

  })

    output$test2 <- renderPrint({

      tabUT()
      ## data <- andelGradKropp()[]
      ## print(data, topn = 20)
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

  callModule(skadeMod, "skade", data = skade)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
