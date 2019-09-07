# Code for getting white and rest analytic datasets for simulation study

# Setup -------------------------------------------------------------------

abhiR::reload()

dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
analytic_data <- read_fst(path(dropdir,'Analytic.fst'))

dbdir <- verifyPaths()
sql_conn <- dbConnect(SQLite(), file.path(dbdir,'USRDS.sqlite3'))

till2009 <- tbl(sql_conn, 'till2009')
from2010 <- tbl(sql_conn,'from2010')
studyids <- tbl(sql_conn, 'StudyIDs')
analytic <- tbl(sql_conn, 'AnalyticData')

# "create table hosp1 as select * from till2009 where USRDS_ID in (select USRDS_ID from Whites)"
# "create table hosp2 as select * from from2010 where USRDS_ID in (select USRDS_ID from Whites)"

# hosp1 <- tbl(sql_conn, 'hosp1')
# hosp2 <- tbl(sql_conn, 'hosp2')
# Fixing the REGION specification -----------------------------------------

a1 <- str_pad(as.character(c(23, 50, 33, 25, 09, 36, 42, 44, 34)), 2, pad='0') # Northeast
a2 <- str_pad(as.character(c(48, 40, 05, 22, 28, 01, 47, 21, 12, 13, 45, 37, 51, 11, 24, 10, 54,78, 72 )), 2, pad='0') # South
a3 <- str_pad(as.character(c(20, 31, 46, 38, 29, 19, 27, 17, 55, 18, 26, 39)), 2, pad = '0') # Midwest
a4 <- str_pad(as.character(c(02, 15, 06, 41, 53, 32, 04, 49, 16, 35, 08, 56, 30, 66, 69, 60,64)), 2, pad='0') # West

analytic_data <- analytic_data %>%
  mutate(REGION = case_when(
    analytic_data$STATE %in% a1 ~ "Northeast",
    analytic_data$STATE %in% a2 ~ 'South',
    analytic_data$STATE %in% a3 ~ "Midwest",
    analytic_data$STATE %in% a4 ~ "West"
  ))


# Adding DISGRPC codes to analytic data --------------------------------------------

disgrpc_code <- readRDS(path(dropdir, 'disgrpc_code.rds')) # See cause_esrd.R
disgrpc <- fst(path(dropdir, 'raw_data.fst'))[,c('USRDS_ID','DISGRPC')] %>%
  distinct() %>%
  left_join(disgrpc_code, by=c('DISGRPC'='Format')) %>%
  mutate(Description = ifelse(Description %in% c('8','**OTHER**'), NA, Description)) %>%
  mutate(Description = fct_other(Description,keep=c('Diabetes','Hypertension','Glomeruloneph.'))) %>%
  rename(ESRD_Cause = Description) 
analytic_data <- analytic_data %>% left_join(disgrpc) # Adding reason for dialysis


# Adding comorbidities ------------------------------------------------
## We wil add albumin, alcohol use and drug use to the set of comorbidities,
## and we'll utilize BMI and Smoking which are already included

medevid <- studyids %>% left_join(tbl(sql_conn, 'medevid')) %>% 
  select(USRDS_ID, ALBUM, ALBUMDT, ALBUMLM,ALCOH, DRUG) %>% 
  collect(n = Inf) %>% 
  mutate_at(vars(ALCOH, DRUG), ~ifelse(. == '', NA, .))
medevid1 <- medevid %>% left_join(analytic_data %>% select(USRDS_ID, FIRST_SE)) %>% 
  filter_at(vars(ALCOH:DRUG), ~!is.na(.)) %>% 
  mutate(ALBUMDT = as.Date(ALBUMDT),
         time_from_se = abs(FIRST_SE - ALBUMDT))
bl <- filter(medevid1, !is.na(ALBUM))
bl %>% count(USRDS_ID) %>% filter(n > 1) %>% left_join(bl) -> bl2
bl2 %>% 
  mutate_at(vars(ALCOH, DRUG), ~ifelse(.=='Y', 1, 0)) %>% 
  group_by(USRDS_ID) %>% 
  filter(time_from_se == min(time_from_se, na.rm=T)) %>% 
  mutate(ALBUM = mean(ALBUM, na.rm=T)) %>%
  mutate_at(vars(ALCOH, DRUG), ~max(., na.rm=T)) %>% 
  ungroup() %>% 
  select(-ALBUMLM, -ALBUMDT) %>%
  distinct() -> bl3
medevid1 <- medevid1 %>% 
  count(USRDS_ID) %>% 
  filter(n==1) %>% 
  left_join(medevid1) %>% 
  select(-ALBUMDT, -ALBUMLM) %>% 
  mutate_at(vars(ALCOH, DRUG), ~ifelse(.=='Y',1,0)) %>% bind_rows(bl3) %>% 
  select(-n, -time_from_se)

blah <- analytic_data %>% left_join(medevid1 %>% select(-FIRST_SE))
blah %>% group_by(lubridate::year(FIRST_SE)) %>% 
  summarize_at(vars(Cancer:Smoke, ALBUM, ALCOH, DRUG), ~mean(is.na(.))*100)
blah %>% summarize_at(vars(Cancer:Smoke, ALBUM, ALCOH, DRUG), ~sum(is.na(.)))


analytic_data <- analytic_data %>% left_join(medevid1 %>% select(-FIRST_SE))

analytic_data <- analytic_data %>% 
  mutate(cens_type2 = case_when(
    cens_type == 0 ~ "Lost to followup",
    cens_type == 1 ~ "Dead",
    cens_type == 2 ~ "Transplant",
    cens_type == 3 ~ "Discontinued",
    TRUE ~ NA_character_)) # NA of type character


# Categorize BMI ----------------------------------------------------------

analytic_data <- analytic_data %>% 
  mutate(BMI2 = case_when(
    BMI <= 19 ~ 'Underweight',
    BMI > 19 & BMI <= 25 ~ 'Normal',
    BMI > 25 & BMI <= 30 ~ 'Overweight',
    BMI > 30 ~ 'Obese'
  ))
  

# Categorizing BMI --------------------------------------------------------

analytic_data <- analytic_data %>% 
  mutate(BMI2 = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 25 ~ "Normal",
    BMI >= 25 & BMI < 30 ~ "Overweight",
    BMI >= 30 ~ "Obese",
    TRUE ~ NA_character_
  )) %>% 
  mutate(BMI2 = as.factor(BMI2))


# Ensuring factors are factors --------------------------------------------

analytic_data <- 
  analytic_data %>% 
  mutate(DRUG = as.factor(DRUG),
         ALCOH = as.factor(ALCOH))

# Create white subset -----------------------------------------------------

analytic_whites <- analytic_data %>% filter(RACE2 == 'White')
assertthat::are_equal(sum(analytic_data$RACE2=='White', na.rm=T), nrow(analytic_whites))

write_fst(analytic_whites, path(dropdir, 'Analytic_Whites.fst'))


# Create subset for rest of racial groups for simulation ------------------

analytic_rest <- analytic_data %>% filter(RACE2 != 'White')
assertthat::are_equal(sum(analytic_data$RACE2!='White', na.rm=T), nrow(analytic_rest))
write_fst(analytic_rest, path(dropdir, 'Analytic_Rest.fst'))

