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
      icon = icon("male"),
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


  ## Filter for og alder
  ##########################
  filterDataAge <- reactive({
    ageFra <- input$alder_in[1]
    ageTil <- input$alder_in[2]

    data <- filterData()[!duplicated(ntrid) & age >= ageFra & age <= ageTil]

  })

  ## Antall missing alder
  ########################
  output$alderNA <- renderText({
    ageNA <- masterFile[!duplicated(ntrid) & is.na(age), .N]
    paste0("Missing: ", ageNA, " personer mangler data for alder og er eksludert i analysen")
  })

  ## Filtert data til analysen
  #############################



  ## Plot Alder og Traume
  #########################
  output$plotAT <- renderPlot({
    ## reactive data
    data <- filterDataAge()

    ## Renser data - bort med NA og -1
    cleanAgeTraume <- data[!is.na(age) & age != -1, .N, keyby = list(age, gender)]

    ## Teller antall kvinner og menn for hver aldersgruppe
    ageMan <- cleanAgeTraume[gender == 1, list(mann = N), key = age]
    ageKvinne <- cleanAgeTraume[gender == 2, list(kvinne = N), key = age]
    ageMK <- merge(ageMan, ageKvinne, all = TRUE)

    ## Erstater NA med 0
    bNA(ageMK)

    ## lage summen for begge kjønn
    ageMK[, alle := mann + kvinne, by = age]

    ## konverterer data til long
    dataLongAK <-melt(ageMK, id.vars="age", measure.vars=c("mann","kvinne","alle"), variable.name="gender", value.name="n")

    ## plot with long data
    plotAT <- ggplot(dataLongAK, aes(age, n, group = gender, color = gender)) +
      geom_line() +
      xlab("Alder") +
      ylab("Antall")

    print(plotAT)

  })

  ## Tabell for Alder og kjønn
  ############################
  output$tabAT <- DT::renderDataTable({

    data <- filterDataAge()

    ## Renser data - bort med NA og -1
    cleanAgeTraume <- data[!is.na(age) & age != -1, .N, keyby = list(age, gender)]

    ## Teller antall kvinner og menn for hver aldersgruppe
    ageMan <- cleanAgeTraume[gender == 1, list(mann = N), key = age]
    ageKvinne <- cleanAgeTraume[gender == 2, list(kvinne = N), key = age]
    ageMK <- merge(ageMan, ageKvinne, all = TRUE)

    ## Erstater NA med 0
    bNA(ageMK)

    ## lage summen for begge kjønn
    ageMK[, alle := mann + kvinne, by = age]

    ## Gir nytt navn
    newNavn <- c("Alder", "Menn", "Kvinner", "Alle")
    data.table::setnames(ageMK, 1:4, newNavn)
    ageMK

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

  ## Virksomhetsdata på sykehus
  ###############################
  valgHosp <- callModule(virk_Mod, "virk", resh)
  callModule(virkPlot, "vplot", valg = valgHosp, data = akutt2)



  ## TEST TEST TEST TEST TEST
  #################################
  output$test <- renderPrint({

    })

    output$testText <- renderPrint({

    })
    #####################################

    session$onSessionEnded(stopApp)
}
