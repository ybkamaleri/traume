body <- dashboardBody(

  ## custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  tabItems(
    tabItem(tabName = "tab_dashboard",
            fluidPage(
              tags$h4("Oversikt informasjon for hele NTR databasen"),
              ## tags$br(),
              fluidRow(
                valueBoxOutput("o_traume"),
                valueBoxOutput("o_ais"),
                valueBoxOutput("o_dead")),
              fluidRow(
                box(width = 12,
                    dygraphOutput("traume_dygraph"))
              ))),
    tabItem(tabName = "tab_rapport",
            tags$h3(textOutput("text_enhetNavn")),
            tags$h5(textOutput("text_Dato")),
            plotOutput("plotAT"),
            verbatimTextOutput("test"),
            verbatimTextOutput("testText")),
    tabItem(tabName = "tab_virk_rap",
            h2("Virksomhetsdata"),
            textOutput("test2"))

  )
)
