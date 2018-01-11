#' Normalizing country
#'
#' @param x Numerical vector denoting country of origin
#'
#' @return vector with unique country value
#' @export
#'
#' @examples
normalize_country <- function(x){
  if (length(x) == 1) {
    return(x)
  }
  if (length(unique(x)) == 2){
    if (sum( is.na(x) ) > 0){
      u <- unique(x)
      uu <- u[!is.na(u)]
      return(as.integer(rep(uu, length(x))))
    } else {
      if (227 %in% x) {
        x <- as.integer(rep(x[x!= 227], length(x))) # keep non-US affiliation
      } else {
        x <- as.integer(rep(sample(x, 1), length(x))) # random choice
      }
      return(x)
    }
  }
  if (length(unique(x)) > 2) {
    if (227 %in% x) {
      x <- as.integer(rep(227, length(x))) # calls all 3-country individuals as US
    }
    return(x)
  }
}