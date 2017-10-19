# Script to verify if there is current access to database directories and to 
# identify the directories where the databases reside

verifyPaths <- function() {
  require(stringr)
  if (Sys.info()['sysname'] == 'Windows') { # Work windows system
    a <- system('wmic logicaldisk get caption,volumename', intern = T)
    ind <- which(unlist(lapply(a, str_detect, 'Home-IRP\\$')))
    ind1 <- which(unlist(lapply(a, str_detect, 'My Book')))
    ind2 <- which(unlist(lapply(a, str_detect, 'ARAASTAT')))
    if (length(c(ind,ind1,ind2)) == 0) {
      print('No access to databases')
      return(NA)
    }
    if (length(ind) > 0) {
      drv <- str_split(a[[ind]], '\\s{2,}')[[1]][1]
      dbdir <- file.path(drv,'Work','Ward','Studies','USRDS','2015 Data','2015 data')
    } else if (length(ind1) > 0) {
      drv <- str_split(a[[ind1]],'\\s{2,}')[[1]][1] # Priority for My Book
      dbdir <- file.path(drv,'NIAMS','Ward','USRDS','Data','2015 data')
    } else {
      drv <- str_split(a[[ind2]], '\\s{2,}')[[1]][1]
      dbdir <- file.path(drv,'NIAMS','Ward','USRDS','Data','2015 data')
    }
    return(dbdir)
  }
  if(Sys.info()['sysname']=='Darwin'){ # Macbook
    a <- dir('/Volumes')
    if(!any(str_detect(a, 'ARAASTAT'))){
      print('No access to database')
      return(NA)
    }
    dbdir = file.path('/Volumes','ARAASTAT','NIAMS','Ward','USRDS','Data','2015 Data')
    return(dbdir)
  }
}