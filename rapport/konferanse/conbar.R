
conbar <- function(data, x, y, ynum,
                   comp, num, aim = NULL,
                   split = NULL,
                   ascending = TRUE,
                   title, ylab,
                   col1, col2, col3,
                   flip = TRUE,
                   ...) {

  library(data.table)
  library(ggplot2)

  ## missing data
  if (missing(data)) {
    stop("'data' must be provided",
      call. = FALSE)
  }

  ## missing x or y
  if (missing(x) | missing(y)) {
    stop("Both 'x' and 'y' should be specified",
      call. = FALSE)
  }


  
  ## x-axis
  data$xvar <- data[, eval(substitute(x))]
  ## yvar
  data$yvar <- data[, eval(substitute(y))]

  ## y label
  data$barlab <- data[, eval(substitute(ynum))]

  ## Title
  if (missing(title)){
    title <- ""
  } else {
    title = title
  }
  
  ## specify denominator (N)
  if (missing(num)){
    data$.xname <- data$xvar
  } else {
    num2 <- deparse(substitute(num))
    data$.xname <- sprintf("%s (N=%s)", data$xvar, data[, get(num2)])
  }

  ## Label y-axis
  if (missing(ylab)){
    ylab <- substitute(y)
    ## ylab <- paste0("Pls specify eg. ylab = ", "\"", "Percentage", "\"")
  } else {
    ylab = ylab
  }

  ## Theme
  ptheme <- theme_bw() +
    theme(
      axis.text = element_text(size = 10), #text for y and x axis
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      axis.title.y = element_blank(), #no title in y axis
      axis.title.x = element_text(size = 12),
      plot.margin = unit(c(0, 2, 1,1), 'cm'),
      plot.title = element_text(size = 14),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

  ## Colour
  if (missing(col1)) {
    col1 <- "lightblue"
  } else {
    col1 = col1
  }

  if(missing(col2)){
    col2 <- "#6baed6"
    colmix <- c(col1, col2)
  } else {
    colmix <- c(col1, col2)
  }

  ## 10% of max value cutoff text placement outside bar
  if (is.null(split)) {
    ysplit <- with(data, 0.1 * max(yvar))
  } else {
    ysplit = with(data, split * max(yvar))
  }

  data$ypos <- ifelse(data$yvar > ysplit, 1, 0)

  ## position and width specification
  position = position_dodge(width = .80)
  width = .80


  ## ## positioning of text i.e ouside bar when 10% of max value.
  ## ymax <- 0.03 * max(data$yvar)
  ## data$txtpos <- ifelse(data$ypos == 0, data$yvar + ymax, data$yvar - ymax)

  ## Ascending order of .xname according to yvar
  if (ascending) {
    data$.xname <- with(data, factor(.xname, levels = .xname[order(yvar)]))
  }

  ## Base plot
  p <- ggplot(data, aes(.xname, yvar))

  ## Aim line color
  if (missing(col3)) {
    col3 = "blue"
  } else {
    col3 = col3
  }

  ## Aim line
  if (!is.null(aim)) {
    p <- p +
      geom_hline(yintercept = aim, color = col3, size = 1, linetype = "dashed")
  }

  ## Compare bar
  if (missing(comp)) {
    p <- p + geom_bar(width = width, stat = 'identity', fill = col1, position = position)
  } else {
    comp <- grep(comp, data$.xname, value = TRUE)
    p <- p + geom_bar(width = width, stat = 'identity', aes(fill = .xname == comp), position = position)
  }

  ## Plot text placement accordingly
  p <- p +
    geom_text(data = data[which(data$ypos == 1), ], aes(label = barlab), hjust = 1.5, position = position, size = 3.5) +
    geom_text(data = data[which(data$ypos == 0), ], aes(label = barlab), hjust = -0.5, position = position, size = 3.5)


  ## Plot everything
  p <- p +
    labs(title = title, y = ylab, x = "") +
    scale_fill_manual(values = colmix, guide = 'none') +
    scale_y_continuous(expand = c(0, 0)) +
    ptheme

  ## Flip plot
  if (flip) {
    p <- p + coord_flip()
  }

  return(p)
}
