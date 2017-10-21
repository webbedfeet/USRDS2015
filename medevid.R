# Extract comorbidities data from medevid

source('lib/reload.R');reload()

library(DBI)
library(RSQLite)
dbdir <- verifyPaths()
if(is.na(dbdir)){
  stop('Disk not available')
}
con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3') )

dbListTables(con)
sort(dbListFields(con, 'medevid'))

medevid = tbl(con, 'medevid')
medevid
