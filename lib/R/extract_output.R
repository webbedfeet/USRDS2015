extract_output <- function(crmodel, refs){
  tmp <- data.frame(summary(crmodel)$conf.int[,-2])
  tmp <- cbind(tmp, summary(crmodel)$coef[,5])
  names(tmp) <- c('HR','LCB','UCB','Pval')
  tmp$variable <- get_variable(row.names(tmp), names(refs))
  tmp$level <- get_level(row.names(tmp), names(refs))
  tmp$level[tmp$variable=='SEX'] <- 'Female'
  tmp$level[tmp$variable=='HISPANIC'] <- 'Hispanic'
  tmp$level <- ifelse(nchar(tmp$level)<2, '', tmp$level)
  return(tmp)
}
