##%######################################################%##
#                                                          #
####                  Transplantation                   ####
#                                                          #
##%######################################################%##

# We've already computed a new survival time, surv_time2, that 
# incorporates loss-to-followup, death and withdrawal only. So 
# the clock doesn't stop for transplants. We'll see how this 
# affects things.


# setup -------------------------------------------------------------------

abhiR::reload()


# Transplant frequency in each condition group ----------------------------

hosp_post_dx <- readRDS(path(dropdir, 'revision_JASN', 'final_hosp_data.rds'))

tx_freq <- map(hosp_post_dx, 
               ~count(., cens_type) %>% 
                 mutate(Percentage = 100*n/sum(n)) %>% 
                 select(-n) %>% 
                 filter(cens_type == 2)) %>% 
  bind_rows(.id = 'Condition') %>% 
  mutate(Condition = condition_code[Condition])


load(path(dropdir, 'revision_JASN','modeling_data.rda'))
# Crude & adjusted models for discontinuation -----------------------------



# Crude & adjusted models for mortality -----------------------------------


