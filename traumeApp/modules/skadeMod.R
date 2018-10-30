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
            selected = 9
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

        ## Tillegg lower extremity (Ben)
        conditionalPanel(condition = 'input.kropp==8', ns = ns,
          selectInput(inputId = ns("til_lowext"),
            label = "Tilleggsuttrekk:",
            choices = list(
              "Alle" = 1,
              "Bekken" = 2,
              "Femur og tibiafrakturer" = 3
            ),
            selected = 1
          )),
        shinyBS::bsTooltip(ns("til_lowext"),
          title = "Referer kodelisten",
          placement = "right",
          trigger = "hover",
          options = list(container = "body")),

        ## Tillegg external and other (Hudskader og andre skader)
        conditionalPanel(condition = 'input.kropp==9', ns = ns,
          selectInput(inputId = ns("til_hud"),
            label = "Tilleggsuttrekk:",
            choices = list(
              "Alle" = 1,
              "Brannskader" = 2,
              "Hypotermi" = 3
            ),
            selected = 1
          )),
        shinyBS::bsTooltip(ns("til_hud"),
          title = "Referer kodelisten",
          placement = "right",
          trigger = "hover",
          options = list(container = "body")),

        ## Deler for Ryggsøyle (Spine)
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
            selected = 1)),
        shinyBS::bsTooltip(id = ns("til_cerv"),
          title = "Se på FAQ for mer info",
          placement = "right",
          trigger = "hover",
          options = list(container = "body")),
        ## Tillegg for Lumbalcolumna
        conditionalPanel(condition = paste0("input['", ns("til_rygg"),
          "'] == 3 && input['", ns("kropp"), "'] == 6"),
          selectInput(inputId = ns("til_lumb"),
            label = "Tilleggsuttrekk (korsrygg):",
            choices = list(
              "Alle" = 1,
              "Isolerte skjelettskader" = 2,
              "Ryggmargsskade" = 3
            ),
            selected = 1)),
        bsTooltip(id = ns("til_lumb"),
          title = "Se på FAQ for mer info",
          placement = "right",
          trigger = "hover",
          options = list(container = "body")),
        ## Tillegg for Thoracalcolumna
        conditionalPanel(condition = paste0("input['", ns("til_rygg"),
          "'] == 4 && input['", ns("kropp"), "'] == 6"),
          selectInput(inputId = ns("til_thor"),
            label = "Tilleggsuttrekk (thorax):",
            choices = list(
              "Alle" = 1,
              "Isolerte skjelettskader" = 2,
              "Ryggmargsskade" = 3
            ),
            selected = 1)),
        bsTooltip(id = ns("til_thor"),
          title = "Se på FAQ for mer info",
          placement = "right",
          trigger = "hover",
          options = list(container = "body"))
      ),

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
      ),

      ## Tekst valgte data
      #####################
      box(
        width = 3,
        background = "light-blue",
        htmlOutput(ns("txt")),
        column(
          id = "Butang",
          width = 6,
          actionButton(ns("regnButton"), "Beregn")),
        column(
          id = "Butang",
          width = 6,
          actionButton(ns("resetButton"), "Reset"))
      )
    ),

    fluidRow(
      box(width = 9,
        tabBox(side = 'left', selected = "Figur", width = 12,
          tabPanel("Figur", plotlyOutput(ns("fig"))),
          tabPanel("Tabell", DT::dataTableOutput(ns("tabell"))))
      ),
      box(id = "Info",
        width = 3,
        htmlOutput(ns("traumeInfo"))
      ))

    ## fluidRow(
    ##   verbatimTextOutput(ns("test"))
    ## ),

    ## fluidRow(
    ##   verbatimTextOutput(ns("test2"))
    ## )
    )
}


#####################
###    SERVER     ###
#####################

skadeSV <- function(input, output, session, valgDT, dataUK, dataSK){

  ## Reset
  ## ======
  initialInputs <- isolate(reactiveValuesToList(input))

  observe({
    # OPTIONAL - save initial values of dynamic inputs
    inputValues <- reactiveValuesToList(input)
    initialInputs <<- utils::modifyList(inputValues, initialInputs)
  })

  observeEvent(input$resetButton, {
    for (id in names(initialInputs)) {
      value <- initialInputs[[id]]
      # For empty checkboxGroupInputs
      if (is.null(value)) value <- ""
      session$sendInputMessage(id, list(value = value))
    }
  })


  ## Tekst til data valg
  output$txt <- renderUI({
    HTML(valgDT$txt)
  })

  ## Filtert data
  ## ============

  ## ## for test uten module
  ## listNTR <- reactive({
  ##   req(input$ulykke)
  ##   valgDT$data[, list(ntrid)]

  ## })

  ## filtert data for å velge ntrid valgDT henter data fra filterModule. Bruk is.null
  ## hvis ingen data ikke er filtert enda
  listNTR <- reactive({

    req(input$ulykke)

    if (is.null(valgDT$data)){
      dataIN <- data.table(ntrid = 0)
    } else {
      dataIN <- as.data.table(valgDT$data)
    }

    dataIN[, list(ntrid)]

  })


  ## Ullyke data
  dataMod <- reactive({
    data <- dataUK[listNTR(), on = c(ntrid = "ntrid")]
    data[!is.na(ntrid) & !duplicated(ntrid), ]
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

    validate(
      need(nrow(valgDT$data) != 0, "Det ikke finnes data for utvalgte parameter")
    )

    regDT = dataMod()[!duplicated(ntrid) & !is.na(ntrid), valgCol, with = FALSE]

    ## For testing
    ## regDT[, alleUT := 1]

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
  filDataUlykke <- reactive({
    colValg <- ulykkeCol()
    regDataUK()[get(colValg) == 1, list(valgCol = get(colValg)),
      keyby = ntrid]
  })


  ## Transport data
  ## Filtert data for transport typer
  ## problem å bruker !is.na(ntrid)
  filDataTrans <- reactive({
    transVar <- "acc_trsp_rd_type"
    alleTrans <-  c(1:7, 99, 999)

    if (as.numeric(input$transport) == 9){
      data <- regDataUK()[get(transVar) %in% alleTrans, list(valgCol = get(transVar)), keyby = ntrid]
    } else {
      data <- regDataUK()[get(transVar) == as.numeric(input$transport), list(valgCol = get(transVar)), keyby = ntrid]
    }
    data
  })

  ## ReactiveVal for subset data
  dataUlykke <- reactiveVal()

  observe({
    selTall <- ifelse(as.numeric(input$ulykke) != 1, 1, 2)
    inDataUK <- switch(selTall,
      filDataUlykke(),
      filDataTrans()
    )

    dataUlykke(inDataUK)
  })


  ####################### SKADE DATA ###################

  ## Data hentes fra filterModule
  ## OBS!! bruk 'aisMix' for å velge skade gradering
  regData <- reactive({
    #her skal det merge med valg ie. filtertdata
    listNTR <- dataUlykke()
    ## setnames(listNTR, 1, "V1") #gir colname for å sikre riktig kolonnevalg
    dataRaw <- dataSK[listNTR, on = c(ntrid = "ntrid")]
    dataRaw[, aisMix := toString(unlist(strsplit(ais, split = ","))), by = ntrid]

    dataMix <- dataRaw[!duplicated(ntrid)]
    dataMix[, ais := NULL] #slett ais siden aisMix inneholder alle ais koder
    dataMix
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

      data <- dataIN[, list(
        n = ifelse(sum(grepl(paste0(".*[", paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]
    }

    ## Valgte kroppsregion
    if (as.numeric(input$kropp) %in% 1:8) {

      data <- dataIN[, list(
        n = ifelse(sum(grepl(paste0("^", valKropp(), ".*[",
          paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]
    }

    ## Valg External and ogher (Hudskader og andre)
    if (as.numeric(input$kropp) == 9){

      data <- dataIN[, list(
        n = ifelse(sum(grepl(paste0("^[90].*[", paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

    }

    data[n == 1, ]

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

      data <- valgKropp()[, list(
        n = ifelse(sum(grepl(paste0("^", kodeTillegg, ".*[",
          paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]
    }

    data[n == 1, ]

  })


  ## Spine deler
  #########################
  ## Minst en av valgte deler + skadegradering
  tilSpine <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    spineValg <- switch(as.character(input$til_rygg),
      '2' = {"^6\\d{2}2.*"},
      '3' = {"^6\\d{2}6.*"},
      '4' = {"^6\\d{2}4.*"}
    )


    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 1){

      data <- valgKropp()

    } else {

      data <- valgKropp()[, list(
        n = ifelse(sum(grepl(paste0(spineValg, "[",
          paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]
    }

    data[n == 1, ]

  })



  #####################################################
  ## VIKTIG - Data kilder må endres med filtret data ##
  #####################################################

  ## Spine Tilleggsuttrekk - Cervicalcolumna
  tilCerv <- reactive({

    dataIN <- tilSpine()

    ## kode å velge
    kode_skjelett <- "^6502[1-3][024678].*[23]$"
    kode_rygg <- "^6402.*[3-6]$"

    ## Lager subset data
    ## kode er skjelettskader og kode1 ryggmargsskade
    dataValg <- dataIN[, list(
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
    dataSK <- dataValg[, list(kode2 = ifelse(kode == 1 & kode1 == 1, 1, 0),
      gender = gender, age = age, kode = kode, kode1 = kode1), by = ntrid]


    if (as.numeric(input$til_cerv) == 1){
      data <- dataIN
    } else if (as.numeric(input$til_cerv) == 2){
      ## bare de med skjelettskader uten rygmargsskade
      data <- dataSK[kode == 1 & kode2 == 0,
        list(n = 1, gender = gender, age = age), by = ntrid]
    } else {
      data <- dataSK[kode1 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
    }

    data[n == 1, ]

  })


  ## Spine tilleggsuttrekk - Lumbalcolumna
  tilLumb <- reactive({

    dataIN <- tilSpine()

    ## kode å velge
    kode_skjelett <- "^6506[1-3][024678].*[23]$"
    kode_rygg <- "^6406.*[3-5]$"


    ## Lager subset data
    ## kode er skjelettskader og kode1 ryggmargsskade
    dataValg <- dataIN[, list(
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
    dataSK <- dataValg[, list(kode2 = ifelse(kode == 1 & kode1 == 1, 1, 0),
      gender = gender, age = age, kode = kode, kode1 = kode1), by = ntrid]

    data <- switch(as.character(input$til_lumb),
      "1" = {dataIN},
      "2" = {dataSK[kode == 1 & kode2 == 0,
        list(n = 1, gender = gender, age = age),
        by = ntrid]},
      "3" = {dataSK[kode1 == 1,
        list(n = 1, gender = gender, age = age),
        by = ntrid]}
    )

    data[n == 1, ]

  })


  ## Spine tilleggsuttrekk - Thoracalcolumna
  tilThor <- reactive({

    dataIN <- tilSpine()

    ## kode å velge
    kode_skjelett <- "^6504[1-3][024678].*[23]$"
    kode_rygg <- "^6404.*[3-5]$"

    ## Lager subset data
    ## kode er skjelettskader og kode1 ryggmargsskade
    dataValg <- dataIN[, list(
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
    dataSK <- dataValg[, list(kode2 = ifelse(kode == 1 & kode1 == 1, 1, 0),
      gender = gender, age = age, kode = kode, kode1 = kode1), by = ntrid]

    data <- switch(as.character(input$til_thor),
      "1" = {dataIN},
      "2" = {dataSK[kode == 1 & kode2 == 0,
        list(n = 1, gender = gender, age = age), by = ntrid]},
      "3" = {dataSK[kode1 == 1,
        list(n = 1, gender = gender, age = age), by = ntrid]}
    )

    data[n == 1, ]

  })


  ## Lower Extremities
  ## =========================
  tilLowext <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    ## Tilleggkoder
    valgBekken <- "^856"
    valgFemur <- "^853"
    valgTibia <- "^854[0-3]"

    ## Kode om bekken og femu / tibiafrakturer
    if (as.numeric(input$til_lowext) != 1){

      data <- valgKropp()[, list(
        bekken = ifelse(sum(grepl(paste0(valgBekken, ".*[",
          paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        femur = ifelse(sum(grepl(paste0(valgFemur, ".*[",
          paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        tibia = ifelse(sum(grepl(paste0(valgTibia, ".*[",
          paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

      data[femur == 1 | tibia == 1 , femtib := 1]

    }

    data <- switch(input$til_lowext,
      "1" = { valgKropp() },
      "2" = { data[, list(n = bekken, gender = gender, age = age, aisMix = aisMix)]},
      "3" = { data[, list(n = femtib, gender = gender, age = age, aisMix = aisMix)]}
    )

    data[n == 1, ]

  })


  ## External and other
  ## ======================
  tilHud <- reactive({

    req(input$skadegrad)

    valgBrann <- "^9120"
    valgHypo <- "^01"

    kodeTillegg <- ifelse(as.numeric(input$til_hud) == 2, valgBrann, valgHypo)

    if (as.numeric(input$til_hud) == 1){

      data <- valgKropp()

    } else {

      data <- valgKropp()[, list(
        n = ifelse(sum(grepl(paste0("^", kodeTillegg, ".*[",
          paste(valSkade(), collapse = ""), "]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        age = age,
        aisMix = aisMix), by = ntrid]

    }

    data[n == 1, ]


  })



  ## hvis tillegg !=1 så velge output fra tillegg input
  ## ellers velger valgKropp

  ###########
  ## Andel ##
  ###########

  ## ## Inkluderer Grad 1 eller ikke til å beregne andel
  ## andelGradAlle  <- reactive({

  ##   dataIN <- regData()

  ##   if (input$skadegrad1){

  ##     andelG <- dataIN[, list(n = ifelse(
  ##       sum(grepl(".*[1-6]$", as.character(unlist(
  ##         strsplit(aisMix, split = ",")))), na.rm=TRUE) != 0, 1, 0)), by = ntrid]

  ##   } else {

  ##     andelG <- dataIN[, list(n = ifelse(
  ##       sum(grepl(".*[2-6]$", as.character(unlist(
  ##         strsplit(aisMix, split = ",")))), na.rm=TRUE) != 0, 1, 0)), by = ntrid]

  ##   }
  ##   andelG[, sum(n, na.rm = TRUE)]
  ## })

  ## Andell med grad 1 eller ikke for spesifiserte kroppsregion

  valgInfo <- eventReactive(input$regnButton, {

    if (as.numeric(input$kropp) == 10) {
      kroppReg <- ".*"
    }

    if (as.numeric(input$kropp) %in% 1:8) {
      kroppReg <- paste0("^", input$kropp, ".*")
    }

    if (as.numeric(input$kropp) == 9) {
      kroppReg <- "^[90].*"
    }

    kroppReg

  })

  andelGradKropp <- eventReactive(input$regnButton, {

    dataIN <- regData()
    kroppReg <- valgInfo()

    if (input$skadegrad1){

      andelG <- dataIN[, list(
        n = ifelse(sum(grepl(paste0(kroppReg, "[1-6]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix,
        age = age), by = ntrid]

    } else {

      andelG <- dataIN[, list(
        n = ifelse(sum(grepl(paste0(kroppReg, "[1-6]$"),
          as.character(trimws(unlist(strsplit(aisMix, split = ","))))),
          na.rm = TRUE) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix,
        age = age), by = ntrid]

    }

    andelG[, sum(n, na.rm = TRUE)]
    ## andelG
  })


  ## Data Ut observe
  ###################

  ############
  ## Tabell ##
  ############
  tabUT <- eventReactive(input$regnButton, {

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


    ## Valg data
    ## ===============================
    ## Head, Face, Neck, Thorax, Upper extremities
    if (as.numeric(input$kropp) %in% c(1:4, 7, 10)){
      dataUT <- valgKropp()
    }

    ## Abdomen
    if (as.numeric(input$kropp) == 5 && as.numeric(input$til_abdomen) %in% 1:3){
      dataUT <- tilAbdomen()
    }

    ## Spine
    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) %in% 1:4) {
      dataUT <- tilSpine()
    }
    ## Tillegg Spine - Cervicalcolumna
    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 2 && as.numeric(input$til_cerv) %in% 1:3){
      dataUT <- tilCerv()
    }
    ## Tillegg Spine - Lumbalcolumna
    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 3 && as.numeric(input$til_lumb) %in% 1:3){
      dataUT <- tilLumb()
    }
    ## Tillegg Spine - Thoracalcolumna
    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 4 && as.numeric(input$til_thor) %in% 1:3){
      dataUT <- tilThor()
    }

    ## Lower extremities (Ben)
    if (as.numeric(input$kropp) == 8 && as.numeric(input$til_lowext) %in% 1:3){
      dataUT <- tilLowext()
    }


    ## External and others (Hud)
    if (as.numeric(input$kropp) == 9 && as.numeric(input$til_hud) %in% 1:3){
      dataUT <- tilHud()
    }

    dataUT

  })


  ## Figur
  ##########################
  ## Viser eller skjule plot
  gg <- reactiveValues(visPlot = FALSE)

  observeEvent(input$regnButton, {
    ## 0 = FALSE og 1 = TRUE
    gg$visPlot <- input$regnButton
  })

  observeEvent(input$resetButton, {
    gg$visPlot <- FALSE
  })

  ## Plot alder og kjønn
  output$fig <- renderPlotly({

    if (gg$visPlot == FALSE) return()

    isolate({
      validate(
        need(nrow(valgDT$data) != 0, "Det finnes ikke data for utvalgte parameter"),
        need(nrow(tabUT()) != 0, "Det finnes ikke data for utvalgte parameter")
      )

      plot <- fun.plotAS(tabUT())
      plot$plot
    })
  })

  ## Tabell for alder og kjønn
  output$tabell <- DT::renderDT({

    if (gg$visPlot == FALSE) return()

    isolate({
      validate(
        need(nrow(valgDT$data) != 0, "Det finnes ikke data for utvalgte parameter"),
        need(nrow(tabUT()) != 0, "Det finnes ikke data for utvalgte parameter")
      )

      plot <- fun.plotAS(tabUT())
      plot$data
    })

  })

  ## Tall som skal vises
  ######################
  prosInfo <- eventReactive(input$regnButton, {
    no1 <- uniqueN(tabUT()$ntrid)
    no2 <- andelGradKropp()
    pros <- format(no1 / no2 * 100, digits = 1)
    paste0("Total (%) : ", no1, " (", pros, "%) av valgt kroppsregion")
  })

  ## Tekst Info
  output$traumeInfo <- renderUI({

    if (gg$visPlot == FALSE) return()

    isolate({

      validate(
        need(nrow(valgDT$data) != 0, "Ingen data!"),
        need(nrow(tabUT()) != 0, "Ingen data!")
      )

      dataMann <- tabUT()[gender == 1, .N]
      dataKvinne <- tabUT()[gender == 2, .N]

      kilde <- paste0("<h3>", "Tall referanse:", "</h3>")
      ## nrTraume <- paste0("<h4>", prosInfo(), "</h4>")
      nrMenn <- paste0("Antall menn: ", dataMann)
      nrKvinner <- paste0("Antall kvinner: ", dataKvinne)

      HTML(paste0(kilde, br(), prosInfo(), br(), nrMenn, br(), nrKvinner))
    })

  })


  ## ##################
  ## ###### TEST ######
  ## ##################

  ## output$test <- renderPrint({
  ##   regDataUK()

  ##    })

  ## output$test2 <- renderPrint({
  ##   tabUT()
  ## })

}
