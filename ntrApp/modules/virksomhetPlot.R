## Virksomhetsrapport plot module
#################################

virkPlotUI <- function(id){
  ns <- NS(id)
  fluidPage(
    plotOutput(ns("ggplot")))
}

virkPlot <- function(input, output, session, valg, data){
  hospNavn <-quote(valg$value())
  datoFra <- quote(valg$datoFra())
  datoTil <- quote(valg$datoTil())

  output$ggplot <- renderPlot({

    valgDato <- data[!duplicated(ntrid) & !is.na(dateSykehus) &
                       Hospital == eval(hospNavn) &
                           dateSykehus >= as.Date(eval(datoFra), format = "%Y-%m-%d") &
                           dateSykehus <= as.Date(eval(datoTil), format = "%Y-%m-%d")]
    valgDag <- valgDato[, dag := weekdays(dateSykehus)]
    ntot <- dim(valgDag)[1] #total dager
    ukeDag <- valgDag[, .(pros = round((.N / ntot) * 100),
                          n = .N), by = dag]

    ## Passer på riktig rekkefølge
    ukeDag$dag <- factor(ukeDag$dag, levels = c("mandag", "tirsdag", "onsdag", "torsdag",
                                                "fredag", "lørdag", "søndag"))


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


    ggplot(ukeDag, aes(dag, pros)) + geom_bar(stat = "identity")

    ggplot(ukeDag) +
      geom_bar(aes(dag, pros), stat = "identity", fill = "#2171b5") +
      scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + #5% space on top
      labs(y = "prosent") +
      barTheme
  })

}
