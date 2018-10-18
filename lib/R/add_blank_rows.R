#' add_blank_rows
#' 
#' add_blank_rows adds rows of NA values at particular specified rows of a data.frame or tibble
#' One can specify either the row indices before which the rows are to be inserted or the 
#' row indices after which the blank rows are to be inserted
#'
#' @param .data A data.frame or tibble
#' @param .before A vector of indices
#' @param .after A vector of indices
#'
#' @return
#' @export
#'
#' @examples
add_blank_rows <- function(.data, .before = NULL, .after = NULL){
  require(tibble)
  if (is.null(.before) & is.null(.after)) {
    stop('Only one of .before and .after needs to be filled')
  }
  if (!is.null(.before)) {
    i = 0
    for (u in sort(.before)) {
      .data <- .data %>% add_row(.before = u + i)
      i <- i + 1
    }
  }
  if (!is.null(.after)) {
    i <- 0
    for (u in sort(.after)) {
      .data <- .data %>% add_row(.after = u + i)
      i <- i + 1
    }
  }
  return(.data)
}
