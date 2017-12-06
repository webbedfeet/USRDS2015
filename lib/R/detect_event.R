detect_event <- function(ch, event_ch, dialysis_code = '[0-9]'){
  bl <- ch %>% stringi::stri_reverse() %>% str_split_fixed(event_ch, 2)
  return(!str_detect(bl[,1], dialysis_code))
}