#' Simulation function to compute YLL and observed survival times
#' 
#' This function mimics the simulation in `sim_fn`, but computes YLL and observed survival times
#' from the simulations to see how much life is gained by delaying discontinuation in the non-white
#' ethnic groups
#'
#' @param dat_list A list of data sets, one for each index condition
#' @param nsim Number of simulations (default = 1000)
#'
#' @return  Two lists, yll and obstime, which have the requisite simulation results for each index condition
#' @export
#'
#' @examples
sim_fn_yll <- function(dat_list, nsim = 1000){
  conds <- names(dat_list)
  yll <- list()
  obstimes <- list()
  set.seed(10385)
  for (cnd in conds) {
    print(paste('Working on', cnd))
    D <- dat_list[[cnd]]
    weib <- survreg(Surv(time_from_event+0.1, cens_type==3)~ # Added 0.1 since weibull is > 0
                      agegrp_at_event + SEX  + time_on_dialysis +
                      factor(REGION)+
                      zscore + comorb_indx,
                    data = D$White,
                    dist = 'weibull')
    D <- map(D,  ~mutate_at(., vars(toc:tow), funs(as.numeric(.) - as.numeric(time_on_dialysis))))
    shp <- 1/weib$scale
    sc <- map(D, ~exp(predict(weib, newdata = ., type = 'lp')))
    D$White <- D$White %>% mutate(new_tow = tow)
    
    rw <- map(sc, ~matrix(rweibull(length(.)*nsim, shape = shp, scale = .), ncol = nsim, byrow = F)) # Generate random numbers
    
    yll[[cnd]] <- list()
    obstimes[[cnd]] <- list()
    
    yll[[cnd]] = foreach (i = 1:nsim,  .packages = c('tidyverse', 'broom', 'survival')) %dopar% {
      for (n in setdiff(names(D), 'White')){
        D[[n]]$new_tow <- rw[[n]][,i]
      }
      D <- map(D, ~mutate(., new_surv_time = pmin(toc, tod, tot, new_tow, na.rm=T)) %>%
                 mutate(new_cens_type = case_when(toc == new_surv_time ~ 0,
                                                  tod == new_surv_time ~ 1,
                                                  tot == new_surv_time ~ 2,
                                                  new_tow == new_surv_time ~ 3)) %>%
                 mutate(new_surv_time = ifelse(new_cens_type==3, new_surv_time + 7, new_surv_time))) # take withdrawal to death
      blah = bind_rows(D)
      blah %>% filter(new_cens_type == 3) %>% mutate(difftime = time_from_event - new_surv_time) %>%
        group_by(Race) %>% summarize(yll_tot = sum(pmax(0, difftime), na.rm = T),
                                     yll_avg = yll_tot / sum(new_cens_type == 3)) %>%
        ungroup()
    }
    obstimes[[cnd]] <- foreach (i = 1:nsim,  .packages = c('tidyverse', 'broom', 'survival')) %dopar% {
      for (n in setdiff(names(D), 'White')){
        D[[n]]$new_tow <- rw[[n]][,i]
      }
      D <- map(D, ~mutate(., new_surv_time = pmin(toc, tod, tot, new_tow, na.rm=T)) %>%
                 mutate(new_cens_type = case_when(toc == new_surv_time ~ 0,
                                                  tod == new_surv_time ~ 1,
                                                  tot == new_surv_time ~ 2,
                                                  new_tow == new_surv_time ~ 3)) %>%
                 mutate(new_surv_time = ifelse(new_cens_type==3, new_surv_time + 7, new_surv_time))) # take withdrawal to death
      blah = bind_rows(D)
      blah %>% group_by(Race) %>% summarize(total_obstime = sum(new_surv_time, na.rm = T)) %>%
        ungroup() %>% spread(Race, total_obstime)
    }
  }
  return(list('obstimes' = obstimes, 'yll' = yll))
}
