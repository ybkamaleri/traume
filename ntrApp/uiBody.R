body <- dashboardBody(

  ## custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  tabItems(
    tabItem(tabName = "tab_dashboard",
            fluidPage(
              ## tags$h4("Oversikt informasjon for hele NTR databasen"),
              ## tags$br(),
              fluidRow(
                valueBoxOutput("o_traume"),
                valueBoxOutput("o_ais"),
                valueBoxOutput("o_dead")),
              fluidRow(
                box(width = 12,
                    dygraphOutput("traume_dygraph"))
              )
            )),
    tabItem(tabName = "tab_rapport",
            fluidPage(
              tags$h3(textOutput("text_enhetNavn")),
              tags$h5(textOutput("text_info")),
              fluidRow(
                tabBox(side = "left", selected = "Figur", width = 12,
                       tabPanel("Figur", plotOutput("plotAT")),
                       tabPanel("Tabell", DT::dataTableOutput("tabAT")))
              ),
              tags$h5(textOutput("alderNA")),
              tags$br(),
              fluidRow(
                ## tags$div(tags$blockquote(textOutput("alderNA"))),
                infoBoxOutput("traume_info"),
                infoBoxOutput("mann_info"),
                infoBoxOutput("kvinne_info")
              )
            )),
    tabItem(tabName = "tab_virk_rap",
            ## tags$h3(textOutput("virkText")),
            ## tags$h5(textOutput("virkDato")),
            ## verbatimTextOutput("test"),
            ## verbatimTextOutput("testText")
            fluidPage(
              tags$h3("Virksomhetsrapport"),
              fluidRow(
                virk_ModUI("virk")),
              fluidRow(
                virkPlotUI("vplot"))
            ))

  )
)
