function(input, output, session) {

  ## ValueBox
  ####################################
  ## Antall traume for valueBox
  sumTraume <- uniqueN(masterFile$ntrid)

  ## valueBox Antall traume
  output$o_traume <- renderValueBox({
    valueBox(
      value = sumTraume,
      subtitle = "Antall traume",
      icon = icon("pie-chart"),
      color = "blue"
    )
  })

  ## valueBox niss > 15
  output$o_ais <- renderValueBox({
    valueBox(
      value = skade[!duplicated(ntrid) & inj_niss > 15, .N],
      subtitle = "Antall NISS > 15",
      icon = icon("heartbeat"),
      color = "blue"
    )
  })

  ## valueBox døde innen 30 dager
  output$o_dead <- renderValueBox({
    valueBox(
      value = intensiv[!duplicated(ntrid) & res_survival == 1, .N],
      subtitle = "Antall registert døde innen 30 dager",
      icon = icon("bed"),
      color = "blue"
    )
  })


  ## Dygraphs for antall traume with timeseries
  ##################################################
  output$traume_dygraph <- renderDygraph({
    ## Time-series antall traume per dag
    dateAll_dt <- masterFile[!is.na(dateAll), .N, by = .(dateAll)]
    dateMann_dt <- masterFile[!is.na(dateAll)][gender == 1,.N, by = .(dateAll)]
    dateKvinne_dt <- masterFile[!is.na(dateAll)][gender == 2,.N, by = .(dateAll)]

    tsTraumeSub <- dateMann_dt[dateAll_dt, on = c(dateAll = "dateAll")]
    tsTraumeAll <- dateKvinne_dt[tsTraumeSub, on = c(dateAll = "dateAll")]

    ## replace NA to 0 with function by number - RASKERE!
    rep0 <- function(DT){
      for (j in seq_len(ncol(DT)))
        set(DT, which(is.na(DT[[j]])),j,0)
    }
    rep0(tsTraumeAll)

    ## Konvertere til timeseries
    tsAlle <- xts::xts(tsTraumeAll$i.N.1, order.by = tsTraumeAll$dateAll)
    tsMann <- xts::xts(tsTraumeAll$i.N, order.by = tsTraumeAll$dateAll)
    tsKvinne <- xts::xts(tsTraumeAll$N, order.by = tsTraumeAll$dateAll)

    timeTraumeAlle <- cbind(tsAlle, tsMann, tsKvinne)

    ## maxDato og minDato for dyRangeSelector()
    maxDato <- strftime(max(dateAll_dt$dateAll))
    minDato <- strftime(zoo::as.yearqtr(as.Date(maxDato)) - 1, frac = 2)

    ## library(dygraphs)
    dygraph(timeTraumeAlle,
            main = "Antall Traume per dag og kjønn",
            ylab = "Antall") %>%
      dySeries("..1", label = "Alle") %>%
      dySeries("..2", label = "Menn") %>%
      dySeries("..3", label = "Kvinner") %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = FALSE) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE, width = 280) %>%
      dyRangeSelector(dateWindow = c(minDato, maxDato))
  })


  ## Valg analysenivå
  ################################
  AnalyseLevel  <- reactive({
    switch(as.character(input$analyse_level_in),
           '2' = unique(resh$RHF),
           '3' = unique(resh$HF),
           '4' = unique(resh$Hospital))
  })

  ## Output analysenivå
  output[["health_level_out"]] <- renderUI({
    helseEnhet <- as.factor(AnalyseLevel())
    selectInput("health_level_in", label = NULL, choices = sort(helseEnhet))
  })

  ## Dato fra og til
  valgDatoFra <- reactive(
    as.Date(as.character(input$tidsrom_in[1]),
            format = "%Y-%m-%d")
  )

  valgDatoTil <- reactive(
    as.Date(as.character(input$tidsrom_in[2]),
            format = "%Y-%m-%d")
  )

  ## Filtret data for Enhet og Dato
  ##################################
  filterData <- reactive({

    datoFra <- valgDatoFra()
    datoTil <- valgDatoTil()

    switch(as.character(input$analyse_level_in),
           '1' = {masterFile[dateAll >= as.Date(datoFra, format = "%Y-%m-%d") &
                               dateAll <= as.Date(datoTil, format = "%Y-%m-%d")]},
           '2' = {masterFile[RHF == input$health_level_in &
                               dateAll >= as.Date(datoFra, format = "%Y-%m-%d") &
                               dateAll <= as.Date(datoTil, format = "%Y-%m-%d")]},
           '3' = {masterFile[HF == input$health_level_in &
                               dateAll >= as.Date(datoFra, format = "%Y-%m-%d") &
                               dateAll <= as.Date(datoTil, format = "%Y-%m-%d")]},
           '4' = {masterFile[Hospital == input$health_level_in &
                               dateAll >= as.Date(datoFra, format = "%Y-%m-%d") &
                               dateAll <= as.Date(datoTil, format = "%Y-%m-%d")]})
  })

  ## Antall missing alder
  ########################
  output$alderNA <- renderText({
    ageNA <- filterData()[!duplicated(ntrid) & is.na(age), .N]
    paste0("Missing: ", ageNA, " personer mangler data for alder og er eksludert i analysen")
  })


  ## tekst for utvalgte enhetNavn og alder
  ########################################
  observeEvent(input$analyse_level_in, {

    output$text_enhetNavn <- renderText({
      if (input$analyse_level_in == 1){
        paste0("Rapport for hele landet")
      }else{
        paste0("Rapport for ",input$health_level_in)
      }
    })
  })

  output$text_info <- renderText({
    ageFra <- input$alder_in[1]
    ageTil <- input$alder_in[2]

    paste0("Valgt tidsrom: ", format(valgDatoFra(), "%d-%m-%Y"),
           " til ", format(valgDatoTil(), "%d-%m-%Y"), ".",
           " Aldersgruppe: ", ageFra, " til ", ageTil, " år")
  })


  ## Filter alder for data som er allerede filtert for tidsrom
  ##############################################################
  filterDataAge <- reactive({
    ageFra <- input$alder_in[1]
    ageTil <- input$alder_in[2]

    filterData()[!duplicated(ntrid) & age >= ageFra & age <= ageTil]

  })

  ## List ntrid filtert for helseenhet, alder og tidsrom
  listNTR <- reactive({filterDataAge()[, .(ntrid = ntrid)]})


  ## Alder og Traume data for plot
  ################################
  ntrData <- reactive({

    data <- filterDataAge()
    if (input$alder_kat){
      dataUT <- plotreg(data, TRUE)
    } else {
      dataUT <- plotreg(data)
    }
    return(dataUT)
  })
  

  ## Plot Alder og Traume
  #########################
  output$plotAT <- renderPlot({

    dataUT <- ntrData()$plot
    print(dataUT)

  })

  ## Tabell for Alder og kjønn
  ############################
  output$tabAT <- DT::renderDataTable({

    dataUT <- ntrData()$data
    dataUT

  })

  ## InfoBox
  ##################
  output$traume_info <- renderInfoBox({
    data <- filterDataAge()
    infoBox("Antall traume", uniqueN(data$ntrid), icon = icon("pie-chart"))
  })

  output$mann_info <- renderInfoBox({
    data <- filterDataAge()[!duplicated(ntrid) & gender == 1, .N]
    infoBox("Antall menn", data, icon = icon("male"))
  })

  output$kvinne_info <- renderInfoBox({
    data <- filterDataAge()[!duplicated(ntrid) & gender == 2, .N]
    infoBox("Antall kvinner", data, icon = icon("female"))
  })

  ## AIS Skadegradering
  ## aisGrad <- callModule(aisMod, "ais", filterDataAge)
  callModule(ulykkeServer, "u_ais", valgDT = filterDataAge, data = ulykke)


  ## Virksomhetsdata på sykehus
  ###############################
  valgHosp <- callModule(virk_Mod, "virk", resh)
  callModule(virkPlot, "vplot", valg = valgHosp, data = akutt2)




  ## TEST TEST TEST TEST TEST
  #################################
  ## list filtert NTR
  ## testData <- reactive({filterDataAge()[, .(ntrid = ntrid)]})

  dataUL <- reactive({ulykke[testData(), on = c(ntrid = "ntrid")]}) ## bruk til ulykke module

  output$test2 <- renderPrint({
    ## str(aisGrad$data)
    ## dim(aisGrad$data)
    ## paste0("min ntrid ",valg$minAge, " og max ntrid ", valg$maxAge)
    listNTR()

  })

  output$test1 <- renderPrint({
    data <- aisGrad$data
    data[, .N, by = .(Hospital, RHF, HF)]
  })
  #####################################

  session$onSessionEnded(stopApp)
}
