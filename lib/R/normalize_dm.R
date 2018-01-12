normalize_dm <- function(x){
  if (length(unique(x)) > 1){
    x <- rep("Y", length(x))
  }
  return(x)
}