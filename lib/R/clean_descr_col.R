clean_descr_col <- function(x){
  u <- unique(x)
  for(n in u){
    x[x==n][-1] <- ''
  }
  return(x)
}