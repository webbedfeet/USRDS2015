# Developing parametric survival models for discontinuation and 
# transplant outcomes for the white subgroup


# Setup -------------------------------------------------------------------

abhiR::reload()
dropdir <- path(find_dropbox(),'NIAMS','Ward','USRDS2015','data')
dir_exists(dropdir)

# Modeling in whites ------------------------------------------------------
# We will model a Weibull regression with dependent variables
# age, sex, 
analytic_whites <- read_fst(path(dropdir, 'Analytic_Whites.fst'))

weib <- psm(Surv(surv_time+0.1, cens_type==3)~
              AGEGRP + SEX + factor(REGION) + 
              rcs(zscore) + ESRD_Cause + 
              DIABETES + Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + 
              Pvasc + Smoke + ALCOH + DRUG + rcs(BMI) + rcs(ALBUM),
            data = analytic_whites,
            dist = 'weibull')


cs_resid <- -log(1 - pweibull(analytic_whites$surv_time,
                              shape = 1/weib$scale,
                              scale = exp(predict(weib, analytic_whites, type='lp'))))
plot(survfit(Surv(cs_resid, analytic_whites$cens_type==3)~1))
lines(qexp(ppoints(length(cs_resid))), 1 - ppoints(length(cs_resid)), col = 'red')


# survreg(Surv(time_from_event+0.1, cens_type==3)~ # Added 0.1 since weibull is > 0
#           agegrp_at_event + SEX  + time_on_dialysis +
#           factor(REGION)+
#           zscore + comorb_indx + ESRD_Cause,
#         data = D$White,
#         dist = 'weibull')