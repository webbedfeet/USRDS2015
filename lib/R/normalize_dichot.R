#' Replacing a dichotomous outcome with its modal value
#' 
#' If an individual has multiple observations and there is inconsistency or missingness
#' in a dichotomous variable, this function will replace all the values with the modal value
#' If the observed values have the same frequency, or they are all missing, the values will be 
#' replaced by NA
#'
#' @param x A character vector with outcomes denoted by "Y" and "N". Any "" are converted to NA first
#' @param prefer.y If Y and N are equal frequencies, choose Y. Otherwise, make NA
#'
#' @return A vector with either all missing values or the modal value observed
#' @export
#'
#' @examples
normalize_dichot <- function(x, prefer.y=T) {
  x[x==''] <- NA
  if(all(is.na(x))) return(x)
  if (length(unique(x[!is.na(x)]))==1) {
    return(rep(unique(x[!is.na(x)]), length(x))) # Only one unique value
  }
  t1 <- table(x)
  if (length(unique(t1))==1) {
    if(prefer.y){
      return(rep("Y", length(x)))
    } else{
      return(as.character(rep(NA, length(x)))) # Both Y and N have same frequency
    }
  }
  prop_y <- mean(x == "Y", na.rm = T)
  if (prop_y > 0.5){
    return(rep("Y", length(x))) # More Y's
  } else {
    return(rep("N", length(x))) # More N's
  }
}