
raphist <- function(data, x, yl, yc, tab = TRUE, tab1, tab2,
                    title, scale, ascending = TRUE,
                   col1, col2, lab1, lab2, xlab = "Title",
                   num, rotate, leg1, leg2, ...) {

  ###################################################
  ## Prepare and restructure data set
  ###################################################
  library(data.table)
  setDT(data)

  ## error message if at least 1 args ie. data, x, yl or yc is missing
  if (missing(data) || missing(x) || missing(yl) || missing(yc)) {
    stop("At least one of four compulsory arguments is missing. Run args(regcom)",
         call. = FALSE)
  }

  ## choose x-axis. "x" argument
  names(data)[names(data) == as.character(substitute(x))] <- "xvar"

  ## choose y-axis for local. "yl" argument
  names(data)[names(data) == as.character(substitute(yl))] <- "ylocal"
  data$ylocal[is.na(data$ylocal)] <- 0 #replace NA with 0

  ## choose y-axis for national. "yc" argument
  names(data)[names(data) == as.character(substitute(yc))] <- "ycomp"
  data$ycomp[is.na(data$ycomp)] <- 0 #replace NA with 0

  ## specify denominator when in percent. "num" argument
  if (missing(num)) {
    data$.xvar <- data$xvar
  } else {
    num <- as.character(substitute(num))
    data$.xvar <- sprintf("%s (n=%s)", data$xvar, data[, num])
  }

  ## Order data 'ascending' argument
  if (ascending) {
    data <- data[order(data$ylocal), ]
  }

  ## New column for reference
  dfrow <- nrow(data)
  data$ref <- seq.int(dfrow)

  ## ## create dummy row for text - ref highest row and "" for var to avoid showing NA in
  ## ## x-axis
  ## data <- rbindlist(list(data, data[NA]))
  ## data[is.na(ref), `:=` (.xvar = "", ref = as.factor(dfrow + 1))]
  ## ref.row <- dfrow + 1 #dummy row for lab1 and lab2

  ## New DF for extra row to include text eg. N or Total
  dfcol <- names(data)
  xdf <- stats::setNames(data.frame(matrix(ncol = length(dfcol), nrow = 1)),
                         dfcol)
  ## dummy ref row for text
  ref.row <- dfrow + 1
  xdf$ref <- ref.row

  ## replace NA to "" to avoid NA is printed in the x-axis
  xdf$.xvar <- ""

  ## Combine data and new DF
  data <- base::rbind(data, xdf)
  data$ref <- as.factor(data$ref)

  ## table location
  if (max(data$ylocal, na.rm = TRUE) > max(data$ycomp, na.rm = TRUE)){
    ypos <- 0.15 * max(data$ylocal, na.rm = TRUE)
    ymax <- max(data$ylocal, na.rm = TRUE)
  } else {
    ypos <- 0.15 * max(data$ycomp, na.rm = TRUE)
    ymax <- max(data$ycomp, na.rm = TRUE)
  }


  ############################
  ## Other parameters
  ############################
  ## y - label
  xlab = xlab

  ## Colour
  if (missing(col1)) {
    col1 <- "lightblue"
  } else {
    col1 = col1
  }

  if (missing(col2)) {
    col2 <- "blue"
  } else {
    col2 = col2
  }

  col3 <- c(col1, col2)

  ## Table text
  if (missing(tab1)) {
    tab1 = substitute(ylocal)
  } else {
    tab1 = substitute(tab1)
  }

  if (missing(tab2)) {
    tab2 = substitute(ycomp)
  } else {
    tab2 = substitute(tab2)
  }

  ## Table labels
  if (missing(lab1)) {
    lab1 = "(n)"
  } else {
    lab1 = lab1
  }

  if (missing(lab2)) {
    lab2 = "(N)"
  } else {
    lab2 = lab2
  }

  ## Title
  if (missing(title)){
    title <- ""
  } else {
    title = title
  }

  ## rotate tabel text
  if (missing(rotate)) {
    rotate = 0
  } else {
    rotate = rotate
  }

  ## x-label
  if (missing(scale)) {
    scale = " "
  } else {
    scale = scale
  }

  ## legend text
  if (missing(leg1)) {
    leg1 = "Lokal (n)"
  } else {
    leg1 = leg1
  }

  if (missing(leg2)) {
    leg2 = "Norge (N)"
  } else {
    leg2 = leg2
  }

  ## positioning of text for table
  ytxt <- ypos + ymax

  ## conditions for y-axis break
  if (ymax < 11) {
    ybreak <- 2
    yline <- ymax
  } else if (ymax < 51) {
    ybreak <- 5
    yline <- ymax
  } else {
    ybreak <- round(0.2 * ymax, -1)
    yline_end <- 0.05 * ytxt
    yline <- round(ytxt - yline_end, -1) #extend y-axis and -1 to round to nearest 10
  }

  ##gap between n and N
  ygap <- 0.1 * ymax

  ##lenght of grid line
  ygrid <- ymax + (0.05 * ymax)

  ##################################
  ## Plotting
  ##################################

  ## plot theme
  ptheme <- theme_classic() +
    theme(
      axis.text = element_text(size = 10), #text for y and x axis
      axis.ticks.y = element_blank(),
      ## axis.line.x = element_line(size = 0.5),
      axis.line = element_blank(),
      axis.title.y = element_blank(), #no title in y axis of plot
      axis.title.x = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      plot.title = element_text(size = 14),
      plot.margin = unit(c(0, 1, 1,1), 'cm')
    )

  ## plot
  p <- ggplot(data) +
    geom_segment(aes(x = ref, xend = ref,
                     y = ygrid, yend = 0), #if yline value is used line can overlap when big numbers
                 size = 0.3, color = "grey70",
                 linetype = "dashed", lineend = "butt") +
    ## cover up the grid for dummy line
    geom_segment(data = data[data$ref == ref.row, ],
                 aes(x = ref, xend = ref, y = ygrid, yend = 0), #lines overlaps when big numbers if yline value is used
                 size = 0.8, color = "white",
                 lineend = "butt") +
    ## 'fill' is used to get legend for geom_bar
    geom_bar(aes(ref, ylocal, fill = leg1), stat = "identity") +
    ## 'color' is used to get legend
    geom_point(aes(ref, ycomp, color = leg2), stat = "identity",
               shape = 18, size = 6) +
    coord_flip() +
    labs(x = xlab) +
    scale_x_discrete(breaks = factor(data$ref), labels = data$.xvar) +
    scale_fill_manual(values = col1) + #for bar
    scale_color_manual(values = col2) + #for point
    ## order in guides to specify order of the legend and not alphabetically
    guides(fill = guide_legend(override.aes = list(shape = NA), order = 1))

  ## justification for table text
  tjust <- 1 #0 left, 1 right and 0.5 middle

  ## plot with theme and axis text
  p <- p + ptheme +
    labs(title = title, y = scale) +
    ## expand=c(0,0) in scale_y_continuous used to place text close to axis
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, yline, ybreak)) +
    geom_segment(aes(y = 0, yend = yline, x = -Inf, xend = -Inf))

  ## Table
  if (tab){
    p <- p +
      geom_text(aes(ref, ytxt, label = eval(tab1)), hjust = tjust) +
      geom_text(aes(ref, ytxt + ygap, label = eval(tab2)), hjust = tjust) +
      annotate("text", x = ref.row, y = ytxt,
               label = lab1, hjust = tjust, angle = rotate) + #include rotation rot1 and rot2
      annotate("text", x = ref.row, y = ytxt + ygap,
               label = lab2, hjust = tjust, angle = rotate)
  }

  return(p)

}
