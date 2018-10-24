## Function for plot alder og kjønn
## Pass på at duplikate ID allerede tatt bort

fun.plotAS <- function(x, kat = FALSE, bykat = 5){

  ## Rense alder
  data <- x[!is.na(age) & age != -1, list(age, gender)]

  ## Interval for alder kategorien
  by5 = bykat

  ## Alder kategorisering
  if (kat){
    ## Alder kategori
    alder.kat <- function(x, lower, upper, by, sep = "-") {
      ## Finne høyeste kategori
      kat <- paste0(seq(lower + by - 1, upper - 1, by = by))
      indTop <- max(length(kat))
      top <- as.numeric(kat[indTop])

      labs <- paste0(c(paste(seq(lower, upper - by, by = by),
                             seq(lower + by - 1, upper - 1, by = by),
                             sep = sep),
                       paste(top + 1, "+", sep = "")), " år")
      cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
          include.lowest = TRUE, right = FALSE, labels = labs)}

    ageMax <- max(as.numeric(data$age), na.rm = TRUE)
    ageMin <- min(as.numeric(data$age), na.rm = TRUE)

    data[, ageValg := alder.kat(age, ageMin, ageMax, by5)]

  } else {

    data[, ageValg := age]
  }


  ageMan <- data[gender == 1, list(mann = .N), by = ageValg]
  ageKvinne <- data[gender == 2, list(kvinne = .N), by = ageValg]
  ageMK <- merge(ageMan, ageKvinne, all = TRUE)

  ## bytt NA med 0
  bNA(ageMK)

  ## Lager total
  ageMK[, alle := mann + kvinne, by = ageValg]

  ## Gir nytt navn
  newNavn <- c("Alder", "Menn", "Kvinner", "Alle")
  data.table::setnames(ageMK, 1:4, newNavn)
  ageMK

  ## konverterer data til long
  dataLongAK <-melt(ageMK, id.vars="Alder",
                    measure.vars=c("Menn","Kvinner","Alle"),
                    variable.name="gender", value.name="n")


  ## Theme
  pthemes <- theme(axis.text = element_text(size = 9, color = "black"), #text for x og y axis
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(size = 0.5),
    axis.line.y = element_blank(),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, color = "grey"),
    legend.position = "bottom",
    legend.justification = c(0,1), #legend bottom left
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key = element_rect(fill = "white")
  )

  ## Farge
  cols <- c("#084594","#6baed6", "#FF7260")

  ## plot with long data
  plotAT <- ggplot(dataLongAK, aes(Alder, n, group = gender, color = gender)) +
    geom_line() +
    xlab("Alder") +
    ylab("Antall") +
    pthemes +
    scale_colour_manual(values = cols)

  return(list(data = ageMK, plot = plotAT))

}
