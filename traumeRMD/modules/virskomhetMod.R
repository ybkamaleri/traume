## Virksomhetsrapport module
############################

virkDataUI <- function(id){

  ns <- NS(id)
  fluidPage(
    fluidRow(
      ## tags$style(type='text/css',
      ##            "/*selectInput font size*/
      ##             .selectize-input { font-size: 20px; line-height: 20px;}
      ##             .selectize-dropdown { font-size: 18px; line-height: 18px;}
      ##             "),
      ## Valg hospital
      box(width = 4,
        height = 100,
        title = "Valg sykehus:",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("hosp"))),

      ## Valg tidsrom
      box(width = 4,
        height = 100,
        title = "Valg tidsrom:",
        status = "primary",
        solidHeader = TRUE,
        dateRangeInput(inputId = ns("virk_date_in"),
          label = NULL,
          start = Sys.Date() - 360, #alt. min date
          end = Sys.Date(),
          separator = "til",
          format = "dd.mm.yyyy",
          startview = "month",
          language = "no",
          weekstart = 1))
    ))
}

virkDataSV <- function(input, output, session, resh) {

  helseEnhet <- as.factor(unique(resh$Hospital))

  ## list sykehusnavn
  output$hosp <- renderUI({
    ns <- session$ns
    selectInput(ns("hosp_in"),
      label = NULL,
      choices = sort(helseEnhet),
      selected = sort(helseEnhet)[1])
  })

  ## Sykehusnavn
  return(
    list(
      value = reactive({input$hosp_in}),
      datoFra = reactive({input$virk_date_in[1]}),
      datoTil = reactive({input$virk_date_in[2]})
    ))

}


## Virksomhetsrapport plot module
#################################

virkPlotUI <- function(id){
  ns <- NS(id)
  fluidPage(
    plotlyOutput(ns("ggplot")))
}

virkPlotSV <- function(input, output, session, valg, data){

  ## henter fra virkDataSV module
  hospNavn <-quote(valg$value())
  datoFra <- quote(valg$datoFra())
  datoTil <- quote(valg$datoTil())

  output$ggplot <- renderPlotly({

    valgDato <- data[!duplicated(ntrid) & !is.na(dateSykehus) &
                       Hospital == eval(hospNavn) &
                         dateSykehus >= as.POSIXct(eval(datoFra), format = "%Y-%m-%d") &
                         dateSykehus <= as.POSIXct(eval(datoTil), format = "%Y-%m-%d")]
    valgDag <- valgDato[, dag := weekdays(dateSykehus)]
    ntot <- dim(valgDag)[1] #total dager
    ukeDag <- valgDag[, .(prosent = round((.N / ntot) * 100),
      n = .N), by = dag]

    ## Passer på riktig rekkefølge
    ukeDag$dag <- factor(ukeDag$dag,
      levels = c("mandag", "tirsdag", "onsdag", "torsdag", "fredag", "lørdag", "søndag"))


    barTheme <- theme(axis.text = element_text(size = 12, color = "black"), #text for x og y axis
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      axis.title.y = element_text(size = 13),
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linetype = 2, color = "grey"),
      legend.position = "none"
    )


    ggplot(ukeDag, aes(dag, prosent)) + geom_bar(stat = "identity")

    virkplot <- ggplot(ukeDag) +
      geom_bar(aes(dag, prosent, antall = n), stat = "identity", fill = "#2171b5") +
      scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + #5% space on top
      labs(y = "prosent") +
      barTheme

    plotly::ggplotly(virkplot, tooltip = c("dag", "prosent", "antall"))
  })

  }
