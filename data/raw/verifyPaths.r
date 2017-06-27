# Script to verify if there is current access to database directories and to 
# identify the directories where the databases reside

library(stringr)
if (Sys.info()['sysname'] == 'Windows') { # Work windows system
  a <- system('wmic volume get name,label', intern = T)
  ind1 <- which(unlist(lapply(a, str_detect, 'My Book')))
  ind2 <- which(unlist(lapply(a, str_detect, 'ARAASTAT')))
  if (length(c(ind1,ind2)) == 0) stop('No access to databases')
  if (length(ind1) > 0) {
    drv <- str_split(a[[ind1]],'\\s{2,}')[[1]][2] # Priority for My Book
  } else {
    drv <- str_split(a[[ind2]], '\\s{2,}')[[1]][2]
  }
  
  dbdir <- file.path(drv,'NIAMS','Ward','USRDS','Data','2015 data')
}