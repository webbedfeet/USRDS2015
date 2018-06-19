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

out_yll_fn <- function(simres){
  yll <- simres$yll

  bl = modify_depth(yll, 2,  ~filter(., Race != 'White') %>%
                      select(Race, yll_avg) %>% spread(Race, yll_avg)) %>%
    modify_depth(1, bind_rows)

  bl %>% map(~summarize_all(., mean, na.rm=T) %>% mutate_all(funs(./30.42))) %>% bind_rows(.id='Condition')
}


out_obstimes_fn <- function(simres, mod_data){
  obstimes = simres$obstimes %>%
    modify_depth(1,bind_rows) %>%
    map(~summarize_all(., mean) %>% gather(Race, obs_times))
  Ns <- map(mod_data, ~map_df(., nrow) %>% gather(Race, N))
  mod_dat0 <- mod_data %>% modify_depth(1, bind_rows)
  nominal_obstimes <- map(mod_dat0, ~mutate(., time_from_event = ifelse(cens_type ==3, time_from_event + 7, time_from_event)) %>%
                            group_by(Race) %>%
                            summarize(nominal_obstime = sum(time_from_event, na.rm=T)) %>%
                            ungroup() %>%
                            mutate(Race = as.character(Race)))
final_tbl <- map2(nominal_obstimes, obstimes, ~inner_join(.x, .y, by='Race')) %>%
  map2(Ns, ~inner_join(.x, .y, by='Race')) %>%
  map(~mutate(., pct_change = 100*(nominal_obstime - obs_times)/nominal_obstime ,
              avg_nominal_obstime = nominal_obstime / N / 30.42, # months
              avg_obstime = obs_times/ N/ 30.42)) %>% # months
  bind_rows(.id = 'Condition') %>%
  clean_cols(Condition) %>%
  set_names(c('Condition','Race','Nominal Obs Time (days)',
              'Sim Obs Time (days)', 'N',
              'Percent change',
              'Avg Nominal Obs Time (months)',
              'Avg Sim Obs Time (months)'))
return(final_tbl)
}
