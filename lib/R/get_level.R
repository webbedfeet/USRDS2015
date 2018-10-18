get_level <- function(x, terms) {
  l <- x
  for(u in terms){
    l <- str_replace(l,u,'')
  }
  return(l)
}
