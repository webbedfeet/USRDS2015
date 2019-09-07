## Revision part 2
## 
## Show the mortality rates for those who discontinued vs those who didn't, 
## by race, in a table.


# Setup -------------------------------------------------------------------

abhiR::reload()
dropdir <- path(abhiR::find_dropbox(), 'NIAMS','Ward','USRDS2015', 'data')

analytic <- read_fst(path(dropdir,'Analytic.fst'))
load(path(dropdir, 'modeling_data.rda'))


# Merging death info with modeling data -----------------------------------

tmp <- analytic %>% 
  select(USRDS_ID, DIED) %>% 
  mutate(dead = ifelse(is.na(DIED), 0,1))

dat <- map(modeling_data, ~left_join(., tmp, by='USRDS_ID'))

# Computing person-time till death or censoring ---------------------------

# Censor at transplant

py <- function(d){
  d <- d %>% 
     mutate(end_date = pmax(surv_date, DIED, na.rm=T)) %>% 
     mutate(end_date = if_else(cens_type==2, as.Date(surv_date), as.Date(end_date))) %>% 
    mutate(pdays = end_date - CLM_FROM) %>%
    mutate(pyrs = as.numeric(pdays / 365.25))
  return(d)
}

dat <- map(dat, py)


# Computing rates by race and discontinuation status ----------------------

rates <- function(d){
  d1 <- d %>% 
    group_by(Race, discontinued = cens_type==3) %>% 
    summarize(tot_py = sum(pyrs),
              tot_death = sum(dead),
              mortality_rate = tot_death/tot_py) %>% 
    ungroup()
  d2 <- d %>% group_by(Race) %>% summarize(perc_discontinued = 100*mean(cens_type==3),
                                           perc_transplant = 100*mean(cens_type==2))
  d <- left_join(d1,d2)
  d <- d %>% select(Race, discontinued, mortality_rate, perc_discontinued, perc_transplant) %>% 
    spread(discontinued, mortality_rate) %>% 
    rename(
      `Percent discontinued` = perc_discontinued,
      `Percent transplant` = perc_transplant,
      `Continued` = `FALSE`,
      `Discontinued` = `TRUE`
    )
  return(d)
}

out <- map(dat, rates) 
out <- out[c('stroke_primary','LuCa', 'dement','thrive')]
names(out) = c('Stroke','Lung Cancer','Dementia','Failure to thrive')
out = bind_rows(out, .id = 'Condition')
out <- clean_cols(out, Condition)
openxlsx::write.xlsx(out, file = 'docs/mortality_rates.xlsx', 
                     creator="Abhijit Dasgupta",
                     colWidths='auto')

map(dat, ~. %>% group_by(cens_type==3) %>% summarize(mortality = mean(dead)))

map(dat, function(d) d %>% group_by(Race,cens_type==3) %>% summarize(mortality=mean(dead)))

