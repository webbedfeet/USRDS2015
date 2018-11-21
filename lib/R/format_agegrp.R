#' Formatting the output of age groups
#' 
#' This function transforms age groups from "[30,40)" to "30-39"
#'
#' @param str_vec A vector of strings
#'
#' @return A vector of strings, with age groups formatted
#' @export
#'
#' @examples
#' x <- c('[20,30)', '[30,40)')
#' format_agegrp(x)
format_agegrp <- function(str_vec){
  bl = function(tst){
    y = str_replace(tst,'\\d{2}\\)', function(u){
      as.character(as.numeric(str_extract(u, '\\d{2}'))-1) # Grab the second number and reduce by 1
    })
    return(str_replace(y, '\\[(\\d{2}),(\\d{2})','\\1-\\2')) # Re-format to a-b format
  }
  str_replace(str_vec, '\\[\\d{2},\\d{2}\\)',bl) # Vectorized output
}