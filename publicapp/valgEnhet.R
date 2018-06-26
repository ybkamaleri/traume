
library(shiny)
library(data.table)

data <- fread("ReshHF.csv", encoding = "Latin-1")

ui3 <- fluidPage(

  headerPanel("NTR Test"),

  sidebarLayout(
    sidebarPanel(
      uiOutput("valgEnhet"),
      uiOutput("valgEnhetNavn"),

      checkboxInput("compare", "Sammenligne med hele landet")
    ),
    mainPanel(

    )
  )
)

helseEnhet <- c("RHF", "HF", "Sykehus")

ser3 <- function(input, output, session) {

  output$valgEnhet <- renderUI({
    radioButtons("enhet", "Valg Enheten",
                 choices = as.list(helseEnhet),
                 selected = NULL)
  })

  output$valgEnhetNavn <- renderUI({
    # if missing input, return to avoid error
    if(is.null(input$enhet)) return()

    valgetEnhet <- input$enhet
    enhetNavn <- data[, sort(unique(get(valgetEnhet)))]

    selectInput("helseNavn", "Helse Enheten",
                choices = enhetNavn)

  })

}

shinyApp(ui = ui3, server = ser3)
