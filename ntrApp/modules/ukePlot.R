## Plot output modele ukedager
##############################

ukePlotOutput <- function(id){
  ns <- NS(id)

  tagList(plotOutput(ns("plot")))
}


ukePlot <- function(input, output, session, valg, data){

  hospNavn1 <- reactive({valg$value})
  datoFra1 <- reactive({valg$datoFra})
  datoTil1 <- reactive({valg$datoTil})

  hospNavn <- hospNavn1()
  datoFra <- datoFra1()
  datoTil <- datoTil1()

  valgDato <- data[!duplicated(ntrid) & !is.na(dateSykehus) & Hospital == get(hospNavn) &
                     dateSykehus >= as.Date(get(datoFra), format = "%Y-%m-%d") &
                       dateSykehus <= as.Date(get(datoTil), format = "%Y-%m-%d")]
  valgDag <- valgDato[, dag := weekdays(dateSykehus)]
  ntot <- dim(valgDag)[1] #total dager
  ukeDag <- valgDag[, .(pros = round((.N / ntot) * 100),
                        n = .N), by = dag]

  ukeDag$dag <- factor(ukeDag$dag, levels = c("mandag", "tirsdag", "onsdag", "torsdag",
                                              "fredag", "lørdag", "søndag"))
  output$plot <- renderPlot({
    ggplot(ukeDag, aes(dag, pros)) + geom_bar(stat = "identity")
  })



}
