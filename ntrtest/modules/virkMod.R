## Virksomhet module i et

## UI
virkUI <- function(id){

  ns <- NS(id)

  fluidPage(
    fluidRow(
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
          start = Sys.Date() - 3660, #alt. min date
          end = Sys.Date() - 1360,
          separator = "til",
          format = "dd.mm.yyyy",
          startview = "month",
          language = "no",
          weekstart = 1))
    ),

    ## Figur
    fluidRow(
    plotly::plotlyOutput(ns("virkplot"))
  )

  )
}


## Server
virkSV <- function(input, output, session, resh, data) {

  library(data.table)

  helseEnhet <- as.factor(unique(resh$Hospital))

  ## list sykehusnavn
  output$hosp <- renderUI({
    ns <- session$ns
    selectInput(ns("hosp_in"),
      label = NULL,
      choices = sort(helseEnhet),
      selected = sort(helseEnhet)[1])
  })

  ## Sykehusnavn og dato
  hospNavn = reactive({input$hosp_in})
  datoFra = reactive({input$virk_date_in[1]})
  datoTil = reactive({input$virk_date_in[2]})

  ## Plotting
  output$virkplot <- plotly::renderPlotly({

    ## progress indikator
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())

    progress$set(message = 'Vent',
      detail = 'kalkulering pågår...')

    for (i in 1:4) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }

    ## Plotting
    valgDato <- data[!duplicated(ntrid) & !is.na(dateSykehus) &
                       Hospital == hospNavn() &
                         dateSykehus >= as.POSIXct(datoFra(), format = "%Y-%m-%d") &
                         dateSykehus <= as.POSIXct(datoTil(), format = "%Y-%m-%d")]
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


    ggplot2::ggplot(ukeDag, aes(dag, prosent)) + geom_bar(stat = "identity")

    vvplot <- ggplot2::ggplot(ukeDag) +
      geom_bar(aes(dag, prosent, n = n), stat = "identity", fill = "#2171b5") +
      scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + #5% space on top
      labs(y = "prosent") +
      barTheme

    plotly::ggplotly(vvplot, tooltip = c("dag", "prosent", "n"))
  })



}
