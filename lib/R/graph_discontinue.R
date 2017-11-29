#' Graph to display time from comorbidity incidence to discontinuation
#'
#' @param d A data frame / tibble with a particular format (see DETAILS)
#'
#' @return A ggplot2 object
#' @export
#' @details 
#' The tibble that is input into this function will have the following columns:
#' 1. race : A factor variable for race
#' 2. toe  : Time of entry into study
#' 3. toc  : Time of incident severe comorbidity
#' 4. tod  : Time of discontinuation
#' Data will be one row per individual
#' @examples
graph_discontinue <- function(d){
  require(tidyverse)
  d <- d %>% 
    mutate_at(starts_with('t'), as.Date)
    mutate(time_to_comorb = toc - toe,
           time_to_disc = tod - toe) %>% 
    arrange(time_to_comorb, time_to_disc) %>% 
    mutate(indx = 1:n())
  ggplot(d) + 
    geom_segment(aes(x = 0, y = indx, xend = time_to_disc, yend = indx ), color='black')+
    geom_segment(aes(x = time_to_comorb, y = index, xend = time_to_disc, yend = indx), color='blue')+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}