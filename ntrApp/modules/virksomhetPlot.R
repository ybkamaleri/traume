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


    ukeDag$dag <- factor(ukeDag$dag, levels = c("mandag", "tirsdag", "onsdag", "torsdag",
                                                "fredag", "lørdag", "søndag"))

    ggplot(ukeDag, aes(dag, pros)) + geom_bar(stat = "identity")
  })

}
