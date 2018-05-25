#' Expanding ICD-9 codes from shortcuts
#'
#' @param x ICD-9 codes or ranges (character) 
#'
#' @return Expansion of valid ICD-9 codes
#' @export
#'
#' @examples
expand_code <- function(x){
  if(x=='') return(NULL)
  if(str_detect(x,'-')){
    y <- str_trim(unlist(str_split(x,'-')))
    yy <- seq(y[1], y[2])
    return(unlist(map(yy, expand_code)))
  }
  if(str_detect(x, 'V')){
    return(x)
  }
  if(str_detect(x, '[xX]')){
    x <- toupper(x)
    x <- str_replace(x,'X', as.character(0:9))
    return(unlist(map(x, expand_code)))
  }
  if(nchar(x) == 3){
    bl = map(seq(paste0(x,'0'), paste0(x,'9')), expand_code)
    return(c(x, unlist(bl)))
  }
  if(nchar(x) == 4){
    return(c(x,seq(paste0(x,'0'),paste0(x,'9'))))
  }
  if(nchar(x)==5){
    return(x)
  }
}

#' Expand a set of ICD-9 shortcuts into a full set of ICD-9 codes
#'
#' @param x Set of ICD-9 specifications (character)
#'
#' @return A vector of ICD-9 codes
#' @export
#'
#' @examples
#' icd9_codes('410-414, V4581')
icd9_codes <- function(x){
  check_string(x)
  short_codes <- str_trim(unlist(str_split(x, ',')))
  codes <- unlist(map(short_codes, expand_code))
  return(codes)
}

