alder.kat <- function(x, lower, upper, by, sep = "-") {
  ## Finne h?yeste kategori
  kat <- paste0(seq(lower + by - 1, upper - 1, by = by))
  indTop <- max(length(kat))
  top <- as.numeric(kat[indTop])
  
  labs <- paste0(c(paste(seq(lower, upper - by, by = by),
                         seq(lower + by - 1, upper - 1, by = by),
                         sep = sep),
                   paste(top + 1, "+", sep = "")), " år")
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      include.lowest = TRUE, right = FALSE, labels = labs)}
