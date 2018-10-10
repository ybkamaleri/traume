sidebar <- dashboardSidebar(width = 250, #tilpase title tekst
  sidebarMenu(
    menuItem("Generell informasjon",
      tabName = "tab_info",
      icon = icon("info-circle"))
  )
)
