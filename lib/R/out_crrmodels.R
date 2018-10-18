out_crrmodels <- function(rdafiles, reflevels, pretty=T){
  ## Collect results for crrmodels across age groups
  models <- list()
  for(i in 1:6){
    if(file.exists(file.path('output','rda',rdafiles[i]))){
      load(file.path('output','rda',rdafiles[i]))
      models[[agegrps[i]]] <- 
        format_output(crrmodel, reflevels, termLabels, pretty=pretty)
    } else {
      models[[agegrps[i]]] <- 
        data.frame('Predictor' = models[[agegrps[1]]]$Predictor,
                   'Level' = models[[agegrps[1]]]$Level,
                   'Result' = rep('', nrow(models[[agegrps[1]]])))
    }
    names(models[[agegrps[i]]])[3] = paste('Age', agegrps[i])
  }
  out_full <- plyr::join_all(models)
  return(out_full)
}