
## Function for rapport barplot. bruke "grid.draw(plotOutPut)"

rapMidt <- function(DT, x, n1, n2, pros1, pros2, ascending = FALSE, line2 = FALSE,lpos=0.92, lgap=6){
  
  ## error message if at least 1 args ie. data, x, yl or yc is missing
  if (missing(DT) || missing(x) || missing(n1) || missing(n2)) {
    stop("At least one of four compulsory arguments is missing. Run args(rapbar)",
         call. = FALSE)
  }
  
  data <- copy(DT)
  data.table::setDT(data)
  
  ## choose x-axis. "x" argument
  data.table::setnames(data, as.character(substitute(x)), "xvar")
  
  ## choose y-axis for local. "n1" argument
  data.table::setnames(data, as.character(substitute(n1)), "n1")
  
  ## choose y-axis for national. "n2" argument
  data.table::setnames(data, as.character(substitute(n2)), "n2")
  
  ## choose y-axis for national. "pros1" argument
  data.table::setnames(data, as.character(substitute(pros1)), "pros1")
  
  ## choose y-axis for national. "pros2" argument
  data.table::setnames(data, as.character(substitute(pros2)), "pros2")
  
  ## Order data 'ascending' argument
  if (ascending) {
    data <- data[order(data$pros1), ]
  } else {
    data <- data[order(data$gos, decreasing = TRUE),]
  }
  
  ## Dummy row for table header
  refrow <- as.character(nrow(data) + 1) #reference to dummy row
  dtrow <- nrow(data)
  data[, ref := as.factor(seq.int(dtrow))]
  data <- data.table::rbindlist(list(data, data[NA])) #lager dummy row with NA
  data[is.na(ref), ref := refrow] #reference to dummy row
  
  ## Tekst til ? bruke for tabell
  data[, text1 := paste0(n1, ";(", pros1, ")"), by = ref]
  data[, text2 := paste0(n2, ";(", pros2, ")"), by = ref]
  
  ## Bytt NA til "" i dummy row s? det ikke skal vises som NA i figuren
  data[ref == refrow, `:=` (xvar = "",
                            text1 = "",
                            text2 = "")]
  
  ## Hvis n1 eller n2 mindre enn 5, tar bort
  data[n1 <= 5, text1 := "n<6"]
  data[n2 <= 5, text2 := "n<6"]
  
  ## Finne h?yste s?yle
  yvar <- data[, list(v1 = max(pros1, na.rm = TRUE), v2 = max(pros2, na.rm = TRUE))]
  ymax <- ifelse(with(yvar, v1 > v2), yvar$v1, yvar$v2)
  
  ## Y-axis for text position
  yText1 <- ymax + ymax * 0.1
  yText2 <- yText1 + lgap
  
  ## conditions for y-axis break
  if (ymax < 11) {
    ybreak <- 2
    yline <- ymax
  } else if (ymax < 51) {
    ybreak <- 5
    yline <- ymax
  } else {
    # ybreak <- round(0.2 * ymax, -1)
    # yline_end <- 0.05 * yText1
    # yline <- round(yText1 - yline_end, -1) #extend y-axis and -1 to round to nearest 10
    ybreak <- 10
    yline <- ymax
  }
  
  ## Other paramenters
  fsize <- 3 #fontsize
  
  ## Farger
  col1 <- "#6baed6"
  col2 <- "#2171b5" #hvis bare en s?yle
  col3 <- "#084594" #Den andre s?yle
  
  ## Theme
  Theme001 <- theme(
    axis.text = element_text(size = 10), #text for y and x axis
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10), size = 10),
    axis.title.x = element_text(margin = margin(t = 10), size = 10),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(0,lpos), #c(0,0) bottom left og c(1,1) top right
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.key.width = unit(1, 'lines'), #width key
    legend.spacing.x = unit(0.3, 'cm'), #avstand mellom keys
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14),
    plot.margin = unit(c(0, 1, 1, 1), 'cm')
  )
  
  
  
  ## Plot
  plotgg1 <- ggplot(data) +
    ## linje mot tallene p? tabell
    geom_segment(aes(x = ref, y = 0, xend = ref, yend = yline), linetype = 2, color = "grey70") +
    ## Dekker top linje
    geom_segment(data = data[ref == as.character(refrow),],
                 aes(x = ref, y = 0, xend = ref, yend = yline), size = 1, color = "white") +
    geom_bar(aes(ref, pros2, fill = "Norge"), stat = "identity") +
    geom_bar(aes(ref, pros1, fill = "Midt"), stat = "identity", width = 0.35) +
    scale_fill_manual(values = c("Norge"=col1, "Midt"=col3), labels=c("Helse Midt", "Hele landet")) +
    labs(y = "prosent", x = "Endring fra innleggelse til utskriving") +
    coord_flip() + guides(fill=guide_legend(reverse = TRUE)) +
    Theme001 
  
  if (line2){
    plotgg2 <- plotgg1 +
      scale_x_discrete(breaks=factor(data$ref),
                       labels=gsub(";", "\n", data$xvar))
  } else {
    plotgg2 <- plotgg1 +
      scale_x_discrete(breaks = factor(data$ref), labels = data$xvar)
  }
  
  plotgg <- plotgg2 +
    ## limit y - axis scale
    scale_y_continuous(expand = c(0,0), breaks = seq(0, yline, ybreak)) +
    geom_segment(aes(y = 0, yend = yline, x = -Inf, xend = -Inf)) +
    ## tabell
    geom_text(aes(ref, yText1, label = gsub(";", "\n", text2)), hjust = 0.5, size = fsize) +
    geom_text(aes(ref, yText2, label = gsub(";", "\n", text1)), hjust = 0.5, size = fsize) +
    annotate("text", x = as.character(refrow), y = yText1,
             label = "Norge \n N (%)", fontface = "bold", size = fsize) +
    annotate("text", x = as.character(refrow), y = yText2,
             label = "Midt \n N (%)", fontface = "bold", size = fsize)
  
  
  ## Save figure ================================
  fig1a <- ggplot_gtable(ggplot_build(plotgg))
  fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
  ## grid.draw(fig1a)
  
  return(fig1a)
}
