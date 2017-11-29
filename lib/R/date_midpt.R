date_midpt <- function(dt1, dt2) {
  # check that dt1 <= dt2
  if (any (dt1 >  dt2, na.rm=T)){
    stop('dt1 must be before dt2')
  }
  dt1 <- as.Date(dt1)
  dt2 <- as.Date(dt2)
  dt_mid <- dt1 + floor((dt2 - dt1)/2)
  return(dt_mid)
}