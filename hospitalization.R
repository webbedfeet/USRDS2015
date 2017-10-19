# Hospitalization data

source('lib/reload.R'); reload()
dbdir = verifyPaths()
sql_conn = src_sqlite(file.path(dbdir,'USRDS.sqlite3'))

till2009 <- tbl(sql_conn, 'till2009')
from2010 <- tbl(sql_conn, 'from2010')


# Stroke ------------------------------------------------------------------

stroke1 <- till2009 %>% 
  select(USRDS_ID, starts_with('HSDIAG'))%>% 
  mutate(PRIM = substr(HSDIAG1,1,3)) %>% 
  filter(PRIM == '430' | PRIM=='431' | PRIM == '432' | PRIM == '433' | PRIM=='434') %>% 
  collect()
stroke2 <- from2010 %>% 
  select(USRDS_ID, starts_with('HSDIAG')) %>% 
  mutate(PRIM = substr(HSDIAG1,1,3)) %>% 
  filter(PRIM == '430' | PRIM=='431' | PRIM == '432' | PRIM == '433' | PRIM=='434') %>% 
  collect()

stroke_primary <- rbind(select(stroke1, USRDS_ID, HSDIAG1), select(stroke2, USRDS_ID, HSDIAG1)) %>% select(USRDS_ID) %>% distinct()


# Stroke with complications -----------------------------------------------

stroke1_compl <- stroke1 %>% gather(diag, code, -USRDS_ID, -HSDIAG1) %>% 
  mutate(code1 = substr(code,1,3)) %>% 
  filter(code1 %in% c('438','342','344')) %>% 
  select(USRDS_ID) %>% distinct()
stroke2_compl <- stroke2 %>% gather(diag, code, -USRDS_ID, -HSDIAG1) %>% 
  mutate(code1 = substr(code,1,3)) %>% 
  filter(code1 %in% c('438','342','344')) %>% 
  select(USRDS_ID) %>% distinct()
stroke_compl <- rbind(stroke1_compl, stroke2_compl) %>% distinct()


# Lung cancer -------------------------------------------------------------

LuCa1 <- till2009 %>% mutate(PRIM = substr(HSDIAG1,1,3)) %>% 
  filter(PRIM=='162') %>% select(USRDS_ID, HSDIAG1) %>% collect()
LuCa2 <- from2010 %>% mutate(PRIM = substr(HSDIAG1,1,3)) %>% 
  filter(PRIM=='162') %>% select(USRDS_ID, HSDIAG1) %>% collect()
LuCa <- rbind(LuCa1, LuCa2)
