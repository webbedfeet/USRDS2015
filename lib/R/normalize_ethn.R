#' Normalize the ethnicity data
#'
#' @param x Numeric vector representing ethnicity
#'
#' @return Normalized data
#' @export
#'
#' @examples
normalize_ethn <- function(x){
  if (length(unique(x)) == 1){
    return(x)
  } else if (length(unique(x[!is.na(x)])) == 1) {
    return(rep(unique(x[!is.na(x)]), length(x))) # problem is NA
  } else if(any(x[!is.na(x)]=='3') & !all(x[!is.na(x)]=='3')) {
    return(rep(as.character(NA), length(x))) # Ambiguity about hispanics
  }  else if (all(x[!is.na(x)] !='3') & length(unique(x[!is.na(x)])) > 1 ) {
    return(rep(min(x, na.rm=T), length(x))) 
  }
}