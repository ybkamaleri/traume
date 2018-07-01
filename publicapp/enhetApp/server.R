
library(shiny)

ser3 <- function(input, output, session) {

  output$valgEnhet <- renderUI({
    radioButtons("enhet", "Valg Enheten",
                 choices = as.list(helseEnhet),
                 selected = NULL)
  })

  output$valgEnhetNavn <- renderUI({
    # if missing input, return to avoid error
    if(is.null(input$enhet)) return()

    if (input$enhet == "Hele landet") {
      valgetEnhet <- NULL
      enhetNavn <- NULL
    } else {
      valgetEnhet <- input$enhet
      enhetNavn <- data[, sort(unique(get(valgetEnhet)))]
    }

    selectInput("helseNavn", "Helse Enheten",
                choices = enhetNavn)

  })

}
