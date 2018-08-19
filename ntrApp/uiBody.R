body <- dashboardBody(

  tabItems(
    tabItem(tabName = "tab_dashboard",
            fluidPage(
              fluidRow(
                valueBoxOutput("o_traume"),
                valueBoxOutput("o_ais"),
                valueBoxOutput("o_dead")),
              fluidRow(
                box(verbatimTextOutput("test")))))

  )
)
