## App user interface
source("uiBody.R")
source("uiHeader.R")
source("uiSidebar.R")

shinydashboard::dashboardPage(header, sidebar, body)
