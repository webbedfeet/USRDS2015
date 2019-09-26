##%######################################################%##
#                                                          #
####             Counterfactual simulations             ####
#                                                          #
##%######################################################%##

## This code develops counterfactual simulated survival times in non-whites that
## represent white behavior in those groups


# Setup -------------------------------------------------------------------

abhiR::reload()
dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

analytic_whites <- read_fst(path(dropdir, 'Analytic_Whites.fst')) %>%
  mutate(REGION = factor(REGION))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)
analytic_rest <- read_fst(path(dropdir, 'Analytic_Rest.fst')) %>%
  mutate(REGION = factor(REGION))
analytic_rest_byagegrp <- split(analytic_rest, analytic_rest$AGEGRP)
analytic <- read_fst(path(dropdir, "Analytic.fst"))

load(path(dropdir, 'whites_models_final.rda'))

library(foreach)
library(parallel)
library(doParallel)
no_cores <- detectCores()-1


# Extract data to compute comorb_indx -------------------------------------

dbdir <- verifyPaths()
sql_conn <- dbConnect(SQLite(), path(dbdir, 'USRDS.sqlite3'))
till2009 <- tbl(sql_conn, 'till2009') # hospitalization data
from2010 <- tbl(sql_conn,'from2010')  # Hospitalization data
studyids <- tbl(sql_conn, 'StudyIDs') # Study IDs for analytic dataset

comorb_codes <- list(
  'ASHD' = '410-414, V4581, V4582',
  'CHF' = '39891, 422, 425, 428, 402X1,404X1, 404X3, V421',
  'CVATIA' = '430-438',
  'PVD' = '440-444, 447, 451-453, 557',
  'Other cardiac' = '420-421, 423-424, 429, 7850-7853,V422,V433',
  'COPD' = '491-494, 496, 510',
  'GI Bleeding' = '4560-4562, 5307, 531-534, 56984, 56985,578',
  'Liver' = '570-571,5721, 5724,5731-5733,V427',
  'Dysrhhythmia' = '426-427,V450, V533',
  'Cancer' = '140-172, 174-208, 230-231, 233-234',
  'Diabetes' = '250, 3572, 3620X, 36641'
) %>% map(icd9_codes)

d_2009 <- studyids %>% left_join(till2009) %>%
  select(USRDS_ID, starts_with('CLM'), starts_with("HSDIAG")) %>% collect(n=Inf)
d_2010 <- studyids %>% left_join(from2010) %>%
  select(USRDS_ID, starts_with('CLM'), starts_with("HSDIAG")) %>% collect(n = Inf)

determine_comorbs <- function(d){
  d %>% select(USRDS_ID,starts_with("CLM"), starts_with("HSDIAG")) %>%
    gather(DIAG, codes, starts_with("HSDIAG")) %>%
    bind_cols(as.data.frame(lapply(comorb_codes, function(x) .$codes %in% x))) %>%
    group_by(USRDS_ID, CLM_FROM,CLM_THRU) %>%
    summarise_at(vars(ASHD:Diabetes), any) %>%
    ungroup()
}

comorbs_2009 <- determine_comorbs(d_2009)
comorbs_2010 <- determine_comorbs(d_2010)


blah <- vector('list',2)
blah[[1]] <- till2009 %>%
  select(USRDS_ID, starts_with('CLM'), starts_with('HSDIAG')) %>%
  collect(n=Inf) %>%
  gather(DIAG, codes, starts_with("HSDIAG")) %>%
  bind_cols(as.data.frame(lapply(comorb_codes, function(x) .$codes %in% x))) %>%
  select(-DIAG, -codes) %>%
  group_by(USRDS_ID, CLM_FROM, CLM_THRU) %>%
  summarize_all(any) %>%
  ungroup()





# Generate linear predictors ----------------------------------------------

library(rms)
lps <- pmap(list(analytic_rest_byagegrp, final_models_disc, final_models_tr),
            function(x,y,z) tibble(disc = predict(y, newdata=x, type='lp'),
                    tr = predict(z, newdata=x, type = 'lp')))
scls <- map2(final_models_disc, final_models_tr,
             ~tibble(disc = .x$scale, tr = .y$scale))


# Inverse CDF for distributions -------------------------------------------

invcdf <- function(u, x = 'weibull'){
  case_when(x == 'weibull' ~ log(-log(u)),
            x == 'lognormal' ~ qnorm(u),
            x == 'loglogistic' ~ log(u/(1-u)))
}


# Simulate -------------------------------------------------------


whites_byage <- map(analytic_whites_byagegrp,
                    function(d){ d %>%
                      select(USRDS_ID,toc:tow, surv_time, cens_type, RACE2) %>%
                      mutate(disc = NA, tr = NA,
                             new_surv_time = surv_time, new_cens_type = cens_type)
                      })

results <- vector('list', 6)
nsim <- 1000
set.seed(10283)
cl <- makeCluster(no_cores)
registerDoParallel(cl)

for(i in 1:6){
  print(i)
  N <- nrow(analytic_rest_byagegrp[[i]])
  results[[i]] <- foreach(j = 1:nsim, .packages = c('tidyverse', 'broom', 'survival'), .combine=rbind) %dopar% {
    # print(j)
    unifs <- matrix(runif(N*2), ncol = 2)
    es <- tibble(disc = invcdf(unifs[,1], final_models_disc[[i]]$dist),
                 tr = invcdf(unifs[,2], final_models_tr[[i]]$dist))
    lp <- lps[[i]]
    scl <- tibble(disc = rep(scls[[i]]$disc, nrow(lp)),
                  tr = rep(scls[[i]]$tr, nrow(lp)))
    R <- exp(lp + scl * es)
    dat <- select(analytic_rest_byagegrp[[i]],
                  USRDS_ID,toc:tow, surv_time, cens_type, RACE2)
    dat <- cbind(dat, R) %>%
      mutate(new_surv_time = pmin(toc, tod, disc, tr, na.rm=T)) %>%
      mutate(  new_cens_type = case_when(
        toc == new_surv_time ~ 0,
        tod == new_surv_time ~ 1,
        tr == new_surv_time ~ 2,
        disc == new_surv_time ~ 3
      )) %>%
      mutate(new_surv_time = ifelse(new_cens_type ==3,
                                    new_surv_time + 7/365.25,
                                    new_surv_time)) %>%
      bind_rows(whites_byage[[i]]) %>%
      mutate(RACE2 = fct_relevel(RACE2, 'White', 'Black','Hispanic','Asian')) %>%
      filter(RACE2 != 'Other') %>%
      mutate(RACE2 = fct_drop(RACE2))

    mod1 <- broom::tidy(
      coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~ RACE2, data = dat)
    ) %>%
      mutate(term = str_remove(term, 'RACE2')) %>%
      select(term, estimate) %>%
      mutate(estimate = exp(estimate))


    return(mod)
    # results[[j]] <- mod
  }
}
names(results) <- names(analytic_rest_byagegrp)
saveRDS(results, file = path(dropdir,'Revision', 'simResults.rds'), compress = T)
stopCluster(cl)

##%######################################################%##
#                                                          #
####                       Plots                        ####
#                                                          #
##%######################################################%##


base <- analytic %>%
  nest(-AGEGRP) %>%
  mutate(mods = map(data, ~coxph(Surv(surv_time, cens_type %in% c(1,3))~RACE2, data=.))) %>%
  mutate(results = map(mods, tidy)) %>%
  select(AGEGRP, results) %>%
  unnest() %>%
  select(AGEGRP, term, estimate) %>%
  mutate(term = str_remove(term, 'RACE2'),
         estimate = exp(estimate))



plt <- map(names(results),
           ~ggplot(results[[.]], aes(x = estimate)) +
             # geom_histogram(bins=20) +
             geom_density()+
             geom_vline(data = dplyr::filter(base, AGEGRP==.),
                        aes(xintercept = estimate),
                        color = 'red') +
             facet_wrap(~term, scales = 'free') +
             theme_bw() +
             labs(x = 'Hazard ratio against whites',
                  y = '')
           )


bl <- str_remove_all(names(results), '\\[|\\(|\\]|\\)') %>%
  str_split(',') %>%
  do.call(rbind,.) %>%
  as_tibble() %>%
  mutate_all(as.numeric) %>%
  mutate(V1 = ifelse(V1 %% 10 == 9, V1+1, V1)) %>%
  unite('labs',c('V1','V2'), sep = ' - ')


for(i in 1:6) plt[[i]] <- plt[[i]] + ggtitle(paste0('Age ',bl$labs[i]))

pdf('graphs/Revision/simResults.pdf')
for(i in 1:6) print(plt[[i]])
dev.off()


for (nm in names(results)){
  plt <- ggplot(results[[nm]], aes(x = estimate)) + geom_histogram() +
    # geom_vline(data = dplyr::filter(base, AGEGRP==nm),
    #            aes(xintercept = estimate),
    #            color = 'red') +
    facet_wrap(~term, scales='free') +
    theme_bw()
}

##%######################################################%##
#                                                          #
####                  Overall estimates                 ####
#                                                          #
##%######################################################%##

## We can pool the estimates  by using weighted averages of the age-stratum estimates
##

simResults <- readRDS(file = path(dropdir,'Revision', 'simResults.rds'))
N <- analytic %>% count(AGEGRP) %>% mutate(perc = n/sum(n))

hrs <- simResults %>% map(select, estimate) %>% do.call(cbind, .) %>% as.matrix()
overall <- hrs %*% N$perc
overall_results <- tibble(term = simResults[[1]]$term, HR = overall[,1])

ggplot(overall_results, aes(x = HR)) + geom_density() + facet_wrap(~term, scales='free')


##%######################################################%##
#                                                          #
####      Compute comorbidity index for these data      ####
#                                                          #
##%######################################################%##

# Compute USRDS comorb score at each hospitalization ----
# Based on Table 2 of Liu, et al,
# Kidney International (2010) 77, 141–151; doi:10.1038/ki.2009.413

# comorb_indx = ASHD + 3*CHF +
#   2 * (CVATIA + PVD + Other.cardiac + COPD +
#          GI.Bleeding + Liver + Dysrhhythmia + Cancer) +
#   Diabetes

comorb_codes <- list(
  'ASHD' = '410-414, V4581, V4582',
  'CHF' = '39891, 422, 425, 428, 402X1,404X1, 404X3, V421',
  'CVATIA' = '430-438',
  'PVD' = '440-444, 447, 451-453, 557',
  'Other.cardiac' = '420-421, 423-424, 429, 7850-7853,V422,V433',
  'COPD' = '491-494, 496, 510',
  'GI.Bleeding' = '4560-4562, 5307, 531-534, 56984, 56985,578',
  'Liver' = '570-571,5721, 5724,5731-5733,V427',
  'Dysrhhythmia' = '426-427,V450, V533',
  'Cancer' = '140-172, 174-208, 230-231, 233-234',
  'Diabetes' = '250, 3572, 3620X, 36641'
) %>% map(icd9_codes)


# Extract baseline clinical data ------------------------------------------
## We want to compute the comorb_indx at the time of start of dialysis
## i.e., at FIRST_SE
## Mapping from existing variables to comorb_indx components
## Components of comorb_indx
##
## ASHD = IHD | COMO_ASHD = Ihd
## CHF = CARFAIL | COMO_CHF = Cardia
## CVATIA = CARFAIL | COMO_CVATIA = Cva
## PVD = PVASC | COMO_PVD = Pvasc
## Other.Cardiac = COMO_OTHCARD
## COPD = PULMON | COMO_COPD = Pulmon
## GI.Bleeding
## Liver
## Dysrhhythmia = DYSRHYT
## Cancer = CANCER | COMO_CANC = Cancer
## Diabetes = DIABETESÒ

## Extract COMO_OTHCARD & DYSRYTH from medevid and sync with analytic
## We'll extract GI.Bleeding and Liver disease from claims data using a 6 month 
## (180 day) window around the data FIRST_SE, following Liu, et al. 
## For any of the comorbidities that have missing values, we'll impute a 0 (not present)

sql_conn <- dbConnect(SQLite(), 'data/raw/USRDS.sqlite2')
new_comorbs <- dbGetQuery(sql_conn,
                          "select USRDS_ID, COMO_OTHCARD, DYSRHYT from medevid")
sid <- dbReadTable(sql_conn, 'StudyIDs')
dbDisconnect(sql_conn)

new_comorbs <- semi_join(new_comorbs, sid) # Restrict to analytic subjects

### Normalize to remove duplicate ids
setDT(new_comorbs)
bl1 <- new_comorbs[,.N,by=USRDS_ID][N==1][new_comorbs, on="USRDS_ID", nomatch=0]
bl1[,N := NULL]
bl1[bl1==''] <- NA
bl2 <- new_comorbs[, .N, by=USRDS_ID][N>1][new_comorbs, on="USRDS_ID", nomatch =0]
bl2[,N := NULL]
bl2[,':='(COMO_OTHCARD=normalize_dichot(COMO_OTHCARD),
          DYSRHYT=normalize_dichot(DYSRHYT)),
    by = USRDS_ID]
bl2 <- unique(bl2)
new_como <- rbind(bl1, bl2)

analytic_dt <- read_fst(path(dropdir, 'Analytic.fst'), as.data.table = T)
analytic_dt <- merge(analytic_dt, new_como, by = 'USRDS_ID', all.x=T)
setDF(analytic_dt)
analytic_dt <- analytic_dt %>%
  mutate_at(vars(Cancer:Pvasc, DIABETES, COMO_OTHCARD, DYSRHYT), function(x) ifelse(x=='Y', 1, 0)) %>%
  mutate(comorb_indx = Ihd + 3*Cardia + 2*(Cva + Pvasc + COMO_OTHCARD + Pulmon + Cancer) + DIABETES)

till2009 <- setDT(read_fst('data/raw/till2009.fst'))[sid, on='USRDS_ID'][!is.na(CLM_FROM)]
from2010 <- setDT(read_fst('data/raw/from2010.fst'))[sid, on='USRDS_ID'][!is.na(CLM_FROM)]
dx_date <- analytic_dt[,c('USRDS_ID','FIRST_SE')]

dat_dx1 <- merge(till2009[,c("USRDS_ID",'CLM_FROM','CLM_THRU')], dx_date, all.x=T, by='USRDS_ID')
dat_dx2 <- merge(from2010[,c('USRDS_ID','CLM_FROM','CLM_THRU')], dx_date, all.x=T, by='USRDS_ID')
dat_dx <- rbind(dat_dx1, dat_dx2)
dat_dx[,':='(CLM_FROM=as.Date(CLM_FROM), CLM_THRU=as.Date(CLM_THRU))]
setkey(dat_dx, 'USRDS_ID','CLM_FROM')
dat_dx[, ':='(tt_from = abs(FIRST_SE - CLM_FROM),
              tt_thru = abs(FIRST_SE - CLM_THRU))][
                ,min_days := as.numeric(pmin(tt_from, tt_thru))
              ]
dat_dx2 <- dat_dx[, .SD[min_days==min(min_days)], by=USRDS_ID]
