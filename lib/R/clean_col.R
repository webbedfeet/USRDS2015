#' Cleaning up a column for presentation output
#' 
#' This function cleans up a column of text data, typically the first column, in a data.frame
#' so that the same entry isn't repeated in the column as values in a second column, also a 
#' character or factor, changes. 
#' 
#' This function is meant to be useful in magrittr pipelines that use non-standard evaluation.
#'
#' @param x A data.frame or tibble object
#' @param colvar The unquoted name of the column to transform
#'
#' @return
#' @export
#'
#' @examples
clean_col <- function(x, colvar){
  require(dplyr)
  colv <- enquo(colvar)
  x %>% group_by(!!colv) %>% mutate(rown = row_number()) %>% ungroup() %>% 
    mutate_at(vars(!!colv), funs(ifelse(rown > 1, '', .))) %>% 
    select (-rown)
}