#' This script aggregates the results of the simulation study on Biowulf
#' All paths are hard-coded based on biowulf configuration

library(tidyverse)
library(furrr)
library(survival)
library(broom)

datadir <- '/data/dasgupab/USRDS2015/output'
fnames <- dir(datadir, full.names=TRUE)

grab_results <- function(f){
    tidy(readRDS(f)) %>% 
        filter(str_detect(term,'RACE2')) %>%
        select(term, estimate) %>%
        mutate(term = str_remove(term, 'RACE2'),
               estimate = exp(estimate))
}

plan(multiprocess)

results = vector('list', 6)
for(i in 1:6){
    print(i)
    resfiles <- fnames[str_detect(fnames, paste0('model',i))]
    results[[i]] <- future_map_dfr(resfiles, grab_results)
}

saveRDS(results, file.path(datadir,'simResultsMult.rds'), 
        compress=T)

agegrp <-fst::read_fst('/data/dasgupab/USRDS2015/data/AnalyticUpdated.fst')%>%
    pull(AGEGRP)

names(results) <- levels(agegrp)

results2 <- bind_rows(results, .id = agegrp)
results2 <- results2 %>%
    mutate(agegrp = as.factor(agegrp)) %>%
    mutate(agegrp = fct_relevel(agegrp, '[18,29]'))

ggplot(results2, aes(x = estimate, fill = term)) +
    geom_histogram(bins=100, alpha=0.2)+
    geom_vline(xintercept=1, linetype=2)+
    facet_grid(agegrp~., scales='free_y')+
    xlab('Hazard ratio') + 
    ylab('Frequency') +
    labs(fill = 'Race') +
    theme_bw()


