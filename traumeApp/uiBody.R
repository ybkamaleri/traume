body <- dashboardBody(
  ## custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  tabItems(
    tabItem(tabName = "tab_info",
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
      )))

)
