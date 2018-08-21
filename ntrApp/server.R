function(input, output, session) {

  ## Valg analysenivå
  AnalyseLevel  <- eventReactive(input$input_analyse_level, {
    switch(as.character(input$input_analyse_level),
           '2' = unique(resh$RHF),
           '3' = unique(resh$HF),
           '4' = unique(resh$Sykehus))
  })

  ## Output analysenivå
  output[["output_health_level"]] <- renderUI({
    helseEnhet <- as.list(AnalyseLevel())
    selectInput("input_health_level", label = "Valg enhet", choices = helseEnhet)
  })

  ## Filtrerer data for Enhet og Dato
  filterData <- eventReactive(input$input_filter, {

    datoFra <- as.character(input$input_date[1])
    datoTil <- as.character(input$input_date[2])

    paste0(datoFra, " til ", datoTil)

  })


  ## Dygraphs for antall traume with timeseries
  ##################################################
  output$traume_dygraph <- renderDygraph({
    ## Time-series antall traume per dag
    timeDataAll <- masterFile[!is.na(timeAll), .N, by = list(timeAll)]
    timeDataTraume <- xts::xts(timeDataAll$N, order.by = timeDataAll$timeAll)
    maxDato <- strftime(max(timeDataAll$timeAll))
    minDato <- strftime(zoo::as.yearmon((maxDato)) - 1, frac = 1)

    dygraph(timeDataTraume, main = "Antall Traume per dag", ylab = "Antall") %>%
      dyRangeSelector(dateWindow = c(minDato, maxDato)) %>%
      dySeries("V1", label = "Antall traume") %>%
      dyLegend(width = 400, show = "always", hideOnMouseOut = FALSE)
  })


  ## Antall traume for valueBox
  sumTraume <- masterFile[unique(ntrid), .N]

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
      value = skade[unique(ntrid),][inj_niss > 15, .N],
      subtitle = "Antall NISS > 15",
      icon = icon("heartbeat"),
      color = "blue"
    )
  })

  ## valueBox døde innen 30 dager
  output$o_dead <- renderValueBox({
    valueBox(
      value = intensiv[unique(ntrid)][res_survival == 1, .N],
      subtitle = "Antall registert døde innen 30 dager",
      icon = icon("user-circle-o"),
      color = "blue"
    )
  })

  session$onSessionEnded(stopApp)
}
