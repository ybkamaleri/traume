body <- dashboardBody(

  tabItems(
    tabItem(tabName = "tab_dashboard",
            fluidPage(
              fluidRow(
                valueBoxOutput("o_traume"),
                valueBoxOutput("o_ais"),
                valueBoxOutput("o_dead")),
              fluidRow(
                box(width = 12,
                    dygraphOutput("traume_dygraph"))
              ))),
    tabItem(tabName = "tab_rapport",
            h2("Rapport"),
            verbatimTextOutput("test")),
    tabItem(tabName = "tab_virk_rap",
            h2("Virksomhetsdata"),
            textOutput("test2"))

  )
)
