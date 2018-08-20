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

  ## Test output
  output$test <- renderPrint({filterData()})

  ## Antall traume
  sumTraume <- masterFile[unique(ntrid), .N]

  ## Antall døde innen 30 dager når svaret er 1 = Ja eller 2 = Nei
  sumDead30 <- intensiv[unique(ntrid)][res_survival == 1, .N]

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
      value = sumDead30,
      subtitle = "Antall registert døde innen 30 dager",
      icon = icon("user-circle-o"),
      color = "blue"
    )
  })

  session$onSessionEnded(stopApp)
}
