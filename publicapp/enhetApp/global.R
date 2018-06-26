
library(shiny)
library(data.table)

data <- fread("ReshHF.csv", encoding = "Latin-1")

helseEnhet <- c("RHF", "HF", "Sykehus")
