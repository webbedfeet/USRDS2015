get_variable <- function(x, term) {
  v <- rep('', length(x))
  for(u in term){
    v[str_detect(x,u)] <- u
  }
  return(v)
}
