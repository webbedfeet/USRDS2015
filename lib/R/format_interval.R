#' Formatting the output of interval factors
#' 
#' This function transforms intervals from "[30,40)" to "30-39" or "30-39.9" depending on the precision
#'
#' @param string A vector of strings
#' @param precision How many decimal places should the data display
#'
#' @return A vector of strings, with age groups formatted
#' @export
#'
#' @examples
#' x <- c('[20,30)', '[30,40)')
#' format_agegrp(x)
format_interval <- function(string, precision = 0){
  string <- str_replace(string, '[\\d\\.]+\\)', 
                  function(y){
                    paste0(as.numeric(str_extract(y, '\\d{2}')) - 10^(-precision),')')
                  })
  return(str_replace(string, '\\[([\\d\\.]+),([\\d\\.]+)\\)', '\\1-\\2'))
}