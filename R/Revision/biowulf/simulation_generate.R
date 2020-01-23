#' This script generates the datasets needed for the simulation study
#' as well as the script files generated from `simulation_template.R`,
#' based on the number of simulations

# Generate data ----

abhiR::reload()
library(here)
library(fs)
biowulf <- here('R','Revision','biowulf')
biowulf_data <- here('R','Revision','biowulf','data')
biowulf_R <- path(biowulf, 'R')
if(!dir_exists(biowulf_data)) dir_create(biowulf_data)
if(!dir_exists(biowulf_R)) dir_create(biowulf_R)

dropdir <- 'P:/Ward/USRDS2015/data'
if(!dir_exists(dropdir)) dropdir <- here('data')

analytic_whites <- read_fst(path(dropdir,'Revision', 'Analytic_Whites2.fst'))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)
analytic_rest <- read_fst(path(dropdir, 'Revision','Analytic_Rest2.fst'))
analytic_rest_byagegrp <- split(analytic_rest, analytic_rest$AGEGRP)
analytic <- read_fst(path(dropdir, 'Revision', "AnalyticUpdated2.fst"))

assertthat::are_equal(nrow(analytic_whites)+nrow(analytic_rest),
                      nrow(analytic))

for(i in 1:length(analytic_whites_byagegrp)){
    write_fst(analytic_whites_byagegrp[[i]], path(biowulf_data, paste0('whites',i,'.fst')))
    write_fst(analytic_rest_byagegrp[[i]], path(biowulf_data, paste0('rest',i,'.fst')))
}

load('data/whites_models_final.rda')
for(i in 1:6){
  white_model_disc <- final_models_disc[[i]]
  white_model_tr <- final_models_tr[[i]]
  save(white_model_disc,white_model_tr,
       file=paste0('data/white_models_final',i,'.rda'),
       compress=T)
}

biowulf_output <- here('R','Revision','biowulf','output')
if(!dir_exists(biowulf_output)) dir_create(biowulf_output)

# Create scripts from template ----

library(glue)
template_script <- readLines(path(biowulf,'simulation_template.R'))

nsim <- 1000
set.seed(20485)
seeds <- round(runif(nsim, 0, 100000))
for(agegrp_no in 1:6){
    for(iter_no in 1:nsim){
        seed <- seeds[iter_no]
        script <- map_chr(template_script, glue)
        writeLines(script,
                   path(biowulf_R, as.character(glue('script{agegrp_no}_{iter_no}.R'))))
    }
}

