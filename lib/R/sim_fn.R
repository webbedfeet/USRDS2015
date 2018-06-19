#' Function to simulate discontinuation pattern of Whites in other groups
#' 
#' This function estimates the discontinuation process among whites for each index condition 
#' dataset using Weibull AFT regression (as implemented in the `survreg` function) and uses it
#' to simulate discontinuation times for each of the other race/ethnic groups under the White model
#' A Cox model is then fit to the simulated data to estimate hazard ratios for discontinuation/death
#' for each ethnic group against whites. 
#'
#' @param dat_list List of data sets by index condition
#' @param nsim Number of simulations
#'
#' @return A list of Cox PH models, one for each index condition
#' @export
#'
#' @examples
sim_fn <- function(dat_list, nsim = 1000){
  conds <- names(dat_list)
  cox_models <- list()
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
    
    cox_models[[cnd]] <- list()
    rw <- map(sc, ~matrix(rweibull(length(.)*nsim, shape = shp, scale = .), ncol = nsim, byrow = F))
    cox_models[[cnd]] <- foreach (i = 1:nsim,  .packages = c('tidyverse', 'broom', 'survival')) %dopar% {
      # if(i %% 100 == 0) print(i)
      for(n in setdiff(names(D), 'White')){
        D[[n]]$new_tow <- rw[[n]][,i]
      }
      D <- map(D, ~mutate(., new_surv_time = pmin(toc, tod, tot, new_tow, na.rm=T)) %>%
                 mutate(new_cens_type = case_when(toc == new_surv_time ~ 0,
                                                  tod == new_surv_time ~ 1,
                                                  tot == new_surv_time ~ 2,
                                                  new_tow == new_surv_time ~ 3)) %>%
                 mutate(new_surv_time = ifelse(new_cens_type==3, new_surv_time + 7, new_surv_time))) # take withdrawal to death
      blah = bind_rows(D)
      # cox_models[[cnd]][[i]] <- broom::tidy(coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~Race, data = blah))
      broom::tidy(coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~Race, data = blah))
    }
  }
  return(cox_models)
}
