dropdir <- file.path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
abhiR::reload()
load(file.path(dropdir,'revision_JASN', 'modeling_data.rda'))
weib_models <- list()
for (cnd in names(modeling_data2)){
  D = modeling_data2[[cnd]]
  weib_models[[cnd]] <- survreg(Surv(time_from_event+0.1, cens_type==3)~ # Added 0.1 since weibull is > 0
                                  agegrp_at_event + SEX  + time_on_dialysis +
                                  REGION+
                                  zscore + comorb_indx + ESRD_Cause,
                                data = D$White,
                                dist = 'weibull')
}
condition_code <- c('stroke_primary' = 'Primary Stroke',
                    'stroke_compl' = 'Complicated Stroke',
                    'LuCa' = 'Lung Cancer',
                    'MetsCa' = 'Metastatic Cancer',
                    'dement' = 'Dementia',
                    'thrive' = 'Failure to thrive'
)

white_data <- map(modeling_data2, ~.[['White']])
css <- map2(weib_models, white_data, ~cox_snell(.x, .y))
css_df <- map2(css, white_data, ~tibble(survtime = .x, cens = .y$cens_type==3))
css_fits <- map(css_df, ~surv_summary(survfit(Surv(survtime, cens)~1, data=.), data=.) %>% 
                  mutate(ideal = 1-pexp(time)))
css_plots <- map(css_fits, 
                 ~ggsurvplot_df(., censor=F, risk.table=F, palette='jco', legend='none', size=2) + 
                   geom_line(aes(x = time, y = ideal), color = 'red', size=2, alpha = 0.5) +
                   labs(y = '')
                   )
for(i in 1:length(css_plots)){
  css_plots[[i]] = css_plots[[i]] + ggtitle(condition_code[names(css_plots)[i]])
}
library(cowplot)
cowplot::plot_grid(plotlist=css_plots[c('stroke_primary','LuCa','dement','thrive')],
                 nrow=2, ncol=2 )
