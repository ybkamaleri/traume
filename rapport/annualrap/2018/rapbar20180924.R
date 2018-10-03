
## Function for rapport barplot. bruke "grid.draw(plotOutPut)"

rapbar <- function(DT, x, n1, n2, pros1, pros2, ascending = TRUE){
# 
# 
#   pkg <- c("data.table", "ggplot2", "directlabels", "cowplot", "gridExtra", "grid")
# 
#   sapply(pkg, require, character.only = TRUE)
# 
#   ## bytt NA med 0
#   bNA <- function(DT, na = 0){
#     for (j in seq_len(ncol(DT)))
#       set(DT,which(is.na(DT[[j]])),j, na)
#   }
#  

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
  }

  ## Dummy row for table header
  refrow <- as.character(nrow(data) + 1) #reference to dummy row
  dtrow <- nrow(data)
  data[, ref := as.factor(seq.int(dtrow))]
  data <- data.table::rbindlist(list(data, data[NA])) #lager dummy row with NA
  data[is.na(ref), ref := refrow] #reference to dummy row

  ## Tekst til å bruke for tabell
  data[, text1 := paste0(n1, ";(", pros1, ")"), by = ref]
  data[, text2 := paste0(n2, ";(", pros2, ")"), by = ref]

  ## Bytt NA til "" i dummy row så det ikke skal vises som NA i figuren
  data[ref == refrow, `:=` (xvar = "",
                            text1 = "",
                            text2 = "")]

  ## Hvis n1 eller n2 mindre enn 5, tar bort
  data[n1 <= 5, text1 := "n<6"]
  data[n2 <= 5, text2 := "n<6"]

  ## Finne høyste søyle
  yvar <- data[, list(v1 = max(pros1, na.rm = TRUE), v2 = max(pros2, na.rm = TRUE))]
  ymax <- ifelse(with(yvar, v1 > v2), yvar$v1, yvar$v2)


  ## Top text position
  yText1 <- ymax + ymax * 0.1
  yText2 <- yText1 + 7

  ## Other paramenters
  fsize <- 3 #fontsize

  ## Farger
  col1 <- "#6baed6"
  col2 <- "#2171b5" #hvis bare en søyle
  col3 <- "#084594" #Den andre søyle

  ## Theme
  Theme001 <- theme(
    axis.text = element_text(size = 10), #text for y and x axis
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(), #no title in y axis of plot
    axis.title.x = element_text(margin = margin(t = 10), size = 10),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.justification = c(0,1), #legend bottom left
    ## legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    ## legend.key = element_rect(fill = "white"),
    legend.key.width = unit(1, 'lines'), #width key
    legend.spacing.x = unit(0.3, 'cm'), #avstand mellom keys
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14),
    plot.margin = unit(c(0, 1, 1, 1), 'cm')
  )

  ## Plot
  plotgg <- ggplot(data) +
    geom_bar(aes(ref, pros2, fill =  "2016", color = "2016"), stat = "identity") +
    ## linje mot tallene på tabell
    geom_segment(aes(x = ref, y = 0, xend = ref, yend = ymax), linetype = 2, color = "grey70") +
    ## Dekker top linje
    geom_segment(data = data[ref == refrow,],
                 aes(x = ref, y = 0, xend = ref, yend = ymax), size = 1, color = "white") +
    ## for prosent 2
    geom_segment(aes(x = ref, y = 0, xend = ref, yend = pros1, color = "2017"),
                 lineend = "butt", size = 10) +
    scale_fill_manual(values = c("2016" = col1), guide = FALSE) + #for bar
    scale_color_manual(values = c("2016" = col1, "2017" = col3)) + #for segment
    scale_x_discrete(breaks = factor(data$ref), labels = data$xvar) +
    labs(y = "prosent") +
    coord_flip() +
    Theme001 +
    ## limit y - axis scale
    scale_y_continuous(expand = c(0,0), breaks = seq(0, ymax, 10)) +
    geom_segment(aes(y = 0, yend = ymax, x = -Inf, xend = -Inf)) +
    guides(color = guide_legend(override.aes = list(fill = "black"))) +
    ## tabell
    geom_text(aes(ref, yText1, label = gsub(";", "\n", text2)), hjust = 0.5, size = fsize) +
    geom_text(aes(ref, yText2, label = gsub(";", "\n", text1)), hjust = 0.5, size = fsize) +
    annotate("text", x = refrow, y = yText1,
             label = "2016 \n N (%)", fontface = "bold", size = fsize) +
    annotate("text", x = refrow, y = yText2,
             label = "2017 \n N (%)", fontface = "bold", size = fsize)


  ## Save figure ================================
  fig1a <- ggplot_gtable(ggplot_build(plotgg))
  fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
  ## grid.draw(fig1a)

  return(fig1a)
}
