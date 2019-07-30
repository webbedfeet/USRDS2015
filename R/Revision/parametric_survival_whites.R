# Developing parametric survival models for discontinuation and 
# transplant outcomes for the white subgroup

# Setup -------------------------------------------------------------------

library(abhiR)
reload()

dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
analytic_data <- read_fst(path(dropdir,'Analytic.fst'))


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


# Create white subset -----------------------------------------------------

analytic_whites <- analytic_data %>% filter(RACE2 == 'White')
nrow(analytic_whites)
analytic_whites <- analytic_whites %>% 
  mutate(cens_type2 = case_when(
    cens_type == 0 ~ "Lost to followup",
    cens_type == 1 ~ "Dead",
    cens_type == 2 ~ "Transplant",
    cens_type == 3 ~ "Discontinued",
    TRUE ~ NA_character_))