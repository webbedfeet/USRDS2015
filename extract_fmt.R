extract_fmt <- function(d){
  require(stringr)
  d <- str_split(d, '\n')[[1]]
  d <- str_squish(d)
  ind1 <- which(str_detect(d, 'Label'))
  ind2 <- max(which(str_detect(d,'USRDS')))
  d1 <- d[(ind1+1):(ind2-1)]
  
  varnames <- character(length(d1))
  varclass <- character(length(d1))
  formats <- matrix(character(2*length(d1)), ncol=2)
  varind <- str_detect(d1, 'Character|Numeric')
  vars <- str_match(d1, '^[A-Z][A-Z0-9_]+')[varind]
  varnames[varind] <- vars
  varclass[varind] <- str_match(d1, 'Character|Numeric')[varind]
  formats[varind] <- str_split_fixed(d1[varind], ' ', 4)[,3:4]
  formats[!varind] <- str_split_fixed(d1[!varind], ' ', 2)
  formats <- cbind(varnames, varclass, formats)
  colnames(formats) <- c('Variable','Type','Format','Description')
  formats <- as_tibble(formats)
  return(formats)
}