
library(shiny)

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
