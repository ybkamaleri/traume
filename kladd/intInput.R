## dynamisk input basert på innhold i kolonne

library(shiny)
library(data.table)

set.seed(1234)

DT <- data.table(
  V1 <- c("Nord", "Midt", "Vest", "Sør", "Sør", "Nord", "Nord", "Nord"),
  V2 <- c("Satu", "Dua", "Tiga", "Empat"),
  V3 <- sample(2:4, 8, replace = TRUE),
  V4 <- c("Ali", "Baba")
)


server <- function(input, output) {

  ## ## denne funker også men gjøres på annen måte
  ## namasyk <- eventReactive(input$nom, {
  ##   if (input$nom == 1){
  ##     namasyk <- unique(DT$V1)
  ##   } else {
  ##     namesyk <- unique(DT$V2)
  ##   }
  ## })

  ## namasyk <- eventReactive(input$nom, {

  ##   ifelse(input$nom == 1, unique(DT$V1),
  ##          ifelse(input$nom == 2, unique(DT$V2),
  ##                 unique(DT$V4)))
  ## })

  namasyk <- eventReactive(input$nom, {

    namasyk <- switch(as.character(input$nom),
                      '1' = unique(DT$V1),
                      '2' = unique(DT$V2),
                      '3' = unique(DT$V4))

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
