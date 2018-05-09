counts <- function(d){
  require(glue)
  n = nrow(d)
  n_uniq = length(unique(d$USRDS_ID))
  glue('# {formatC(n, format="d", big.mark=",")} obs, {formatC(n_uniq, format="d", big.mark=",")} uniques')
}