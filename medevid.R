# Extract comorbidities data from medevid

ProjTemplate::reload()
# library(DBI)
# library(RSQLite)
dbdir <- verifyPaths()
if(is.na(dbdir)){
  stop('Disk not available')
}
con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3') )

dbListTables(con)
sort(dbListFields(con, 'medevid'))

medevid = tbl(con, 'medevid')
medevid


dbDisconnect(con)
