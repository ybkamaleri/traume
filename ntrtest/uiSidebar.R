sidebar <- shinydashboard::dashboardSidebar(width = 250, #tilpase title tekst
  collapsed = FALSE, #skjule sidebar TRUE
  sidebarMenu(
    menuItem("Generell informasjon",
      tabName = "tab_info",
      icon = icon("info-circle")),

    menuItem("Filter data",
      tabName = "tab_filter",
      icon = icon("database")),

    ## tags$hr(), #linje

    ## menuItem("Ulykke",
    ##   tabName = "tab_ulykke",
    ##   icon = icon("bar-chart")),

    ## menuItem("Akutt",
    ##   tabName = "tab_akutt",
    ##   icon = icon("bar-chart")),

    ## menuItem("Intensiv",
    ##   tabName = "tab_intensiv",
    ##   icon = icon("bar-chart")),

    menuItem("Skadegradering",
      tabName = "tab_skadegrad",
      icon = icon("bar-chart")),

    tags$hr(), #linje

    menuItem("Virksomhetsrapport",
      tabName = NULL,
      icon = icon("h-square"),
      menuSubItem("Uke dager",
        tabName = "tab_virk"),
      menuSubItem("Standard rapport",
        tabName = "tab_rap")
    ),

    tags$hr(),

    menuItem("FAQ",
      tabName = "tab_manual",
      icon = icon("question-circle"))

    ## menuItem("Test",
    ##   tabName = "tab_test",
    ##   icon = icon("wrench"))
  )
)
