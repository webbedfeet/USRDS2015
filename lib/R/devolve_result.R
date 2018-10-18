devolve_result <- function(x){
  require(stringr)
  HR = as.numeric(str_match(x, '^[\\d\\.]+'))
  y <- data.frame(str_match(x, '\\(([\\d\\.]+), ([\\d\\.]+)\\)')[,-1], stringsAsFactors = F)
  y[] <- lapply(y, as.numeric)
  names(y) <- c('LCB','UCB')
  y = cbind(HR=HR, y)
  return(y)
}
