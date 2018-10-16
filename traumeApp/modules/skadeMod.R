############### Module UI #######################

skadeUI <- function(id){

  ns  <- NS(id)

  fluidPage(
    fluidRow(
      ## Ulykke typer
      ##################
      box(width = 3,
        status = "primary",
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
          ))),

      ## Kroppsregioner
      #########################
      box(width = 3,
        status = "primary",
        title = "Kroppsregioner",
        selectInput(inputId = ns("kropp"),
          label = NULL,
          choices = list(
            "Alle" = 10,
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
        conditionalPanel(condition = 'input.kropp==5', ns = ns,
          selectInput(inputId = ns("til_abdomen"),
            label = "Tilleggsuttrekk:",
            choices = list(
              "Alle" = 1,
              "Leverskader" = 2,
              "Miltskader" = 3
            ),
            selected = 1
          )),
        ## Deler for Ryggsøyle
        conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 6"),
          selectInput(inputId = ns("til_rygg"),
            label = "Spesifisert deler:",
            choices = list(
              "Alle" = 1,
              "Cervicalcolumna" = 2,
              "Lumbalcolumna" = 3,
              "Thoracalcolumna" = 4
            ),
            selected = 1
          )),
        ## Tillegg for Cervicalcolumna
        conditionalPanel(condition = paste0("input['", ns("til_rygg"),
          "'] == 2 && input['", ns("kropp"), "'] == 6"),
          selectInput(inputId = ns("til_cerv"),
            label = "Tilleggsuttrekk (nakke):",
            choices = list(
              "Alle" = 1,
              "Isolerte skjelettskader" = 2,
              "Ryggmargsskade" = 3
            ),
            selected = 1),
          bsTooltip(id = ns("til_cerv"),
            title = "Telling for minst en av de allerede spesifiserte AIS koder. Filter for Skadegradering blir ikke benyttes",
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
            title = "Telling for minst en av de allerede spesifiserte AIS koder. Filter for Skadegradering blir ikke benyttes",
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
            title = "Telling for minst en av de allerede spesifiserte AIS koder. Filter for Skadegradering blir ikke benyttes",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")))),

      ## Skadegradering
      #######################
      box(width = 3,
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
          selected = list(2,3,4,5,6)
        ),
        checkboxInput(inputId = ns("skadegrad1"),
          label = "Andel inkluderer skadegrad 1",
          value = FALSE),
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



###################### SERVER ###############################

skadeSV <- function(input, output, session, valgDT, dataUK, dataSK){


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
    dataDT <- dataUK #ulykke data
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
  regDataUK <- reactive({

    regDT = dataMod()[!duplicated(ntrid) & !is.na(ntrid), valgCol, with = FALSE]

    ## Legg alle type ulykke - alleUT : alle ulykke typer
    #######################################################
    regDT[, alleUT := {
      v1 <- unlist(.SD) #ungroup .SDcols
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
    regDataUK()[get(colValg) == 1, list(valgCol = get(colValg)),
      keyby = ntrid]
  })


  ## Transport data
  ## Filtert data for transport typer
  ## problem å bruker !is.na(ntrid)
  filDataTrans <- eventReactive(input$transport, {
    transVar <- "acc_trsp_rd_type"
    alleTrans <-  c(1:7, 99, 999)

    if (as.numeric(input$transport) == 9){
      data <- regDataUK()[get(transVar) %in% alleTrans, list(valgCol = acc_trsp_rd_type), keyby = ntrid]
    } else {
      data <- regDataUK()[get(transVar) == as.numeric(input$transport), list(valgCol = acc_trsp_rd_type), keyby = ntrid]
    }
    data
  })

  ## ReactiveVal for subset data
  dataUlykke <- reactiveVal(NULL)

  observe({
    inDataUK  <- ifelse(as.numeric(input$ulykke) != 1, filDataUlykke(), filDataTrans())
    dataUlykke(inDataUK)
  })


  ####################### SKADE DATA ###################

  ## Data hentes fra filterModule
  ## OBS!! bruk 'aisMix' for å velge skade gradering
  regData <- reactive({
    #her skal det merge med valg ie. filtertdata
    listNTR <- as.data.table(dataUlykke())
    setnames(listNTR, 1, "V1") #gir colname for å sikre riktig kolonnevalg
    dataRaw <- dataSK[listNTR, on = c(ntrid = "V1")]
    dataRaw[, aisMix := toString(unlist(strsplit(ais, split = ","))), by = ntrid]

    dataMix <- dataRaw[!duplicated(ntrid)]
    dataMix[, ais := NULL] #slett ais siden aisMix inneholder alle ais koder
  })


  valKropp <- reactive({as.numeric(input$kropp)}) #kroppregion
  valSkade <- reactive({as.numeric(input$skadegrad)}) #skadegrad


  ## Kroppsregioner
  #########################
  ## Minst en av valgte deler + skadegradering
  valgKropp  <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL
    dataIN <- regData()

    ## Alle kroppsregioner
    if (as.numeric(input$kropp) == 10) {

      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0(".[", paste(valSkade(), collapse = ""), "]$"),
          as.character(toString(trimws(unlist(strsplit(aisMix, split = ","))))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

    } else {

      ## Valgte kroppsregion
      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[", paste(valSkade(), collapse = ""), "]$"),
          as.character(toString(trimws(unlist(strsplit(aisMix, split = ","))))))) != 0, 1, 0),
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
      data <- valgKropp()[, list(valg = ifelse(
        sum(grepl(paste0("^", kodeTillegg, ".*[", paste(valSkade(), collapse = ""), "]$"),
          as.character(toString(trimws(unlist(strsplit(aisMix, split = ","))))))) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

      data[, n := valg]

    }


    dataUT <- data[n == 1]
    return(dataUT)
  })


  ## Spine deler
  #########################
  ## Minst en av valgte deler + skadegradering
  tilSpine <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    spineValg <- switch(as.character(input$til_rygg),
      '2' = "^6\\d{2}2.*",
      "3" = "^6\\d{2}6.*",
      "4" = "^6\\d{2}4.*"
    )


    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 1){

      data <- valgKropp()

    } else {

      data <- valgKropp()[, list(valg = ifelse(
        sum(grepl(paste0(spineValg, "[", paste(valSkade(), collapse = ""), "]$"),
          as.character(toString(trimws(unlist(strsplit(aisMix, split = ",")))))), na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

      data[, n := valg]

    }

    dataUT <- data[n == 1]
    return(dataUT)
  })



  #####################################################
  ## VIKTIG - Data kilder må endres med filtret data ##
  #####################################################

  ## Spine Tilleggsuttrekk - Cervicalcolumna
  tilCerv <- eventReactive(input$til_cerv, {

    dataIN <- tilSpine()

    kode_skjelett <- "^6502[1-3][024678].*[23]$"
    kode_rygg <- "^6402.*[3-6]$"

    ## Lager subset data
    ## kode er skjelettskader og kode2 ryggmargsskade

    kode_skjelett <- "^6502[1-3][024678].*[23]$"
    kode_rygg <- "^6402.*[3-6]$"

    ## Lager subset data
    ## kode er skjelettskader og kode1 ryggmargsskade
    dataSK <- dataIN[, list(
      kode = ifelse(
        sum(grepl(kode_skjelett,
          trimws(unlist(strsplit(aisMix, split = ",")))), na.rm = TRUE) != 0, 1, 0),
      kode1 = ifelse(
        sum(grepl(kode_rygg,
          trimws(unlist(strsplit(aisMix, split = ",")))), na.rm = TRUE) != 0, 1, 0),
      ntrid = ntrid,
      gender = gender,
      age = age), by = ntrid]

    ## kode1 == 1 hvis begge skjelettskader og ryggmargsskade
    dataSK[, kode2 := ifelse(kode == 1 & kode1 == 1, 1, 0), by = ntrid]

    if (as.numeric(input$til_cerv) == 1){
      data <- dataIN
    } else if (as.numeric(input$til_cerv) == 2){
      ## bare de med skjelettskader uten rygmargsskade
      data <- dataSK[kode == 1 & kode2 == 0,
        list(n = 1, gender = gender, age = age), by = ntrid]
    } else {
      data <- dataSK[kode1 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    }


    dataUT <- data[n == 1]
    return(dataUT)

  })


  ## ## Spine tilleggsuttrekk - Lumbalcolumna
  ## tilLumb <- eventReactive(input$til_lumb, {

  ##   dataIN <- regData()

  ##   kode_skjelett <- "^6506[1-3][024678].*[23]$"
  ##   kode_rygg <- "^6406.*[3-5]$"

  ##   ## kode er skjelettskader og kode2 ryggmargsskade
  ##   dataSK <- dataIN[!is.na(ntrid), list(
  ##     kode = ifelse(
  ##       sum(grepl(kode_skjelett,
  ##         trimws(unlist(strsplit(aisMix, split = ",")))), na.rm = TRUE) != 0, 1, 0),
  ##     kode2 = ifelse(
  ##       sum(grepl(kode_rygg,
  ##         trimws(unlist(strsplit(aisMix, split = ",")))), na.rm = TRUE) != 0, 1, 0),
  ##     ntrid = ntrid,
  ##     gender = gender,
  ##     age = age), by = ntrid]


  ##   #kode1 0 hvis begge skjelettskader og ryggmargsskade
  ##   dataSK[, kode1 := kode, by = ntrid]
  ##   dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]

  ##   data <- switch(as.character(input$til_lumb),
  ##     "1" = tilSpine(),
  ##     "2" = dataSK[kode1 == 1,
  ##       list(n = 1, gender = gender, age = age),
  ##       by = ntrid],
  ##     "3" = dataSK[kode2 == 1,
  ##       list(n = 1, gender = gender, age = age),
  ##       by = ntrid]
  ##   )

  ##   dataUT <- data[n == 1]
  ##   return(dataUT)
  ## })


  ## ## Spine tilleggsuttrekk - Thoracalcolumna
  ## tilThor <- eventReactive(input$til_thor, {

  ##   dataIN <- regData()

  ##   kode_skjelett <- "^6504[1-3][024678].*[23]$"
  ##   kode_rygg <- "^6404.*[3-5]$"

  ##   ## kode er skjelettskader og kode2 ryggmargsskade
  ##   dataSK <- dataIN[, list(
  ##     kode = ifelse(
  ##       sum(grepl(kode_skjelett,
  ##         trimws(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
  ##     kode2 = ifelse(
  ##       sum(grepl(kode_rygg,
  ##         trimws(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
  ##     ntrid = ntrid,
  ##     gender = gender,
  ##     age = age), by = ntrid]

  ##   #kode1 0 hvis begge skjelettskader og ryggmargsskade
  ##   dataSK[, kode1 := kode, by = ntrid]
  ##   dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]

  ##   if (as.numeric(input$til_thor) == 1){
  ##     data <- tilSpine()
  ##   } else if (as.numeric(input$til_thor) == 2){
  ##     data <- dataSK[kode1 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
  ##   } else {
  ##     data <- dataSK[kode2 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
  ##   }

  ##   dataUT <- data[n == 1]
  ##   return(dataUT)
  ## })


  ## ## hvis tillegg !=1 så velge output fra tillegg input
  ## ## ellers velger valgKropp

  ## ###########
  ## ## Andel ##
  ## ###########

  ## ## Inkluderer Grad 1 eller ikke til å beregne andel
  ## andelGradAlle  <- reactive({

  ##   dataIN <- regData()

  ##   if (input$skadegrad1){
  ##     andelG <- dataIN[, list(n = ifelse(
  ##       sum(grepl(".*[2-6]$", as.character(unlist(
  ##         strsplit(aisMix, split = ","))))) != 0, 1, 0)), by = ntrid]

  ##   } else {

  ##     andelG <- dataIN[, list(n = ifelse(
  ##       sum(grepl(".*[1-6]$", as.character(unlist(
  ##         strsplit(aisMix, split = ","))))) != 0, 1, 0)), by = ntrid]
  ##   }
  ##   andelG[, sum(n, na.rm = TRUE)]
  ## })

  ## ## Andell med grad 1 eller ikke for spesifiserte kroppsregion
  ## andelGradKropp <- reactive({

  ##   dataIN <- regData()

  ##   if (input$skadegrad1){

  ##     andelG <- dataIN[, list(n = ifelse(
  ##       sum(grepl(paste0("^", valKropp(), ".*[2-6]$"),
  ##         as.character(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
  ##       gender = gender, aisMix = aisMix), by = ntrid]

  ##   } else {

  ##     andelG <- dataIN[, list(n = ifelse(
  ##       sum(grepl(paste0("^", valKropp(), ".*[1-6]$"),
  ##         as.character(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
  ##       gender = gender, aisMix = aisMix), by = ntrid]

  ##   }

  ##   data <- andelG[, sum(n, na.rm = TRUE)]
  ##   return(data)
  ## })

  ## ## Data Ut observe
  ## ###################

  ## ############
  ## ## Tabell ##
  ## ############
  tabUT <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    ## progress indikator
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())

    progress$set(message = 'Vent',
      detail = 'kalkulering pågår...')

    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }

    ##   ## Valg data
    ##   ## if (as.numeric(input$kropp) %in% c(1:4, 10)){
    ##   ##   dataUT <- valgKropp()
    ##   ## } else if (as.numeric(input$kropp) == 5 & as.numeric(input$til_abdomen) %in% 1:3){
    ##   ##   dataUT <- tilAbdomen()
    ##   ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) %in% 1:4) {
    ##   ##   dataUT <- tilSpine()
    ##   ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 2 & as.numeric(input$til_cerv) %in% 1:3){
    ##   ##   dataUT <- tilCerv()
    ##   ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 3 & as.numeric(input$til_lumb) %in% 1:3){
    ##   ##   dataUT <- tilLumb()
    ##   ## } else if (as.numeric(input$kropp) == 6 & as.numeric(input$til_rygg) == 4 & as.numeric(input$til_thor) %in% 1:3){
    ##   ##   dataUT <- tilThor()
    ##   ## }


    ## Valg data
    if (as.numeric(input$kropp) %in% c(1:4, 10)){
      dataUT <- valgKropp()
    }

    if (as.numeric(input$kropp) == 5 && as.numeric(input$til_abdomen) %in% 1:3){
      dataUT <- tilAbdomen()
    }

    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) %in% 1:4) {
      dataUT <- tilSpine()
    }

    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 2 && as.numeric(input$til_cerv) %in% 1:3){
      dataUT <- tilCerv()
    }

    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 3 && as.numeric(input$til_lumb) %in% 1:3){
      dataUT <- tilLumb()
    }

    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 4 && as.numeric(input$til_thor) %in% 1:3){
      dataUT <- tilThor()
    }

    return(dataUT)

  })


  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({
    ## str(regData())
    str(dataUlykke())

  })

  output$test2 <- renderPrint({
    ## str(valgKropp())
    str(regData())

  })

}
