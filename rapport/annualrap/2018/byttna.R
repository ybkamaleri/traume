## bytt NA med 0
byttNA <- function(DT, na = 0){
  library(data.table)
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j, na)
}

