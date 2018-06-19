#' Converting AFT coefficients to HR in the Weibull model
#'
#' @param Results of a weibull regression formatted by broom::tidy
#'
#' @return A tibble containing variables and HR with 95% CI
#' @export
#'
#' @examples
aft_to_hr <- function(x){
  require(glue)
  sc = tail(x,1)$estimate
  x = x[1:(nrow(x)-1),] %>% 
    filter(!str_detect(term,'Intercept'))
  x <- x %>% select(term, estimate, starts_with('conf')) %>% 
    gather(variable, value, -term) %>% 
    mutate(value = exp(-value/sc)) %>% 
    mutate(value = ifelse(value < 1e-3, format(value, digits=2, scientific=F),
                          round(value, digits=2))) %>% 
    spread(variable, value) %>% 
    mutate(out = glue('{estimate} ({conf.high}, {conf.low})')) %>% 
    select(term, out)
  return(x)
}
