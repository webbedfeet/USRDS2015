clean_cols <- function(x, ..., arrange = F){
  colv = quos(...)
  x <- x %>%
    mutate_at(vars(!!!colv),as.character) # make sure columns are characters
  if(arrange){
    x <- x %>% arrange(!!!colv) # Ensure right order of columns
  }
  for(ii in rev(seq_along(colv))){
    indx = duplicated(select(x, !!!colv[ii:1]))
    x <- x %>% mutate_at(vars(!!colv[[ii]]), funs(ifelse(indx, '', .)))
  }
  return(x)
}
