#' ---
#' title: Ingesting data from SAS files into SQLite (and MonetDB)
#' author: "Abhijit Dasgupta (abhijit.dasgupta@nih.gov)"
#' date: 2017-11-22
#' Copyright: Public domain
#' ---

ProjTemplate::reload()
dbdir = verifyPaths()
hosp_data <- file.path(dbdir, '2015 Hospital/hosp')
is.date <- function(x) inherits(x, 'Date')

fp = file.path(hosp_data,dir(hosp_data, pattern='sas7bdat'))
format_file <- file.path(dbdir, '2015 Core','core','formats64.sas7bcat')
# con1 <- dbConnect(MonetDBLite(),dbdir)
con2 <- dbConnect(SQLite(),file.path(dbdir,'USRDS.sqlite3'))
for(u in fp){
  print(u)
  x <- read_sas(u, format_file)
  x <- x %>% mutate_if(is.date, as.character) # Store dates as character, to prevent ambiguity
  print('Done reading SAS data')
  if(stringr::str_detect(u, '2010')){
    # dbWriteTable(con1, 'from2010',x, overwrite=T)
    dbWriteTable(con2, 'from2010',x, overwrite=T)
  } else {
    # dbWriteTable(con2,'till2009', x, overwrite=T)
    dbWriteTable(con2, 'till2009', x, overwrite=T)
  }
  rm(x)
  gc()
}


datdir <- file.path(dbdir,'2015 Core')
if(!dir.exists(file.path(datdir, 'core'))) {
  unzip(file.path(datdir, 'core.zip'), exdir = file.path(datdir, 'core'))
}
coredata = file.path(dbdir,'2015 Core/core')

filenames = dir(coredata, pattern='sas7bdat')
filestems = str_replace(filenames, '.sas7bdat','')
fp = file.path(coredata, filenames)
for(i in 1:length(fp)){
  print(fp[i])
  tst <- try(x <-  read_sas(fp[i], format_file))
  print('Done reading SAS data')
  if(class(tst) == 'try-error') next
  x <- x %>% mutate_if(is.date, as.character)
  # dbWriteTable(con1, filestems[i], x, overwrite =T)
  # print('....writing Monet')
  dbWriteTable(con2, filestems[i], x, overwrite=T)
  print('...writing RSQLite')
  rm(x)
  gc()
}

# dbDisconnect(con1, shutdown=T)
dbDisconnect(con2)
# Ingesting data ----------------------------------------------------------
# mdb_conn <- src_monetdblite(dbdir)
# # sql_conn <- src_sqlite('USRDS.sqlite3')
#
# src_tbls(mdb_conn)
# # src_tbls(sql_conn)
#
# dbDisconnect(mdb_conn$con, shutdown = TRUE)
# dbDisconnect(sql_conn$con)
