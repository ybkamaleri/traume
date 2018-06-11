## dynamisk input basert på innhold i kolonne

library(shiny)
library(data.table)

set.seed(1234)

DT <- data.table(
  v1 <- c("Nord", "Midt", "Vest", "Sør", "Sør", "Nord", "Nord", "Nord"),
  v2 <- rep(c("Satu", "Dua"), 8),
  v3 <- sample(2:4, 8, replace = TRUE)
)


server <- function(input, output) {

  namasyk <- eventReactive(input$nom, {
    if (input$nom = 1){
      namasyk <- unique(DT$V1)
    } else {
      namesyk <- unique(DT$V2)
    }
  })

  output[["syke2"]] <- renderUI({

    nama <- as.list(namasyk())
    selectInput("sykeIn", "Valg Sykehus", choices = nama)

  })

  output$syk3 <- renderText(input$sykeIn)
}


ui <- fluidPage(
  titlePanel("Test dynamisk input"),

  sidebarPanel(
    numericInput("nom", "Pilih nombor", value = 1:3),
    uiOutput("syke2")

  ),

  mainPanel(
    textOutput("syk3")
  )
)


shinyApp(ui, server)
