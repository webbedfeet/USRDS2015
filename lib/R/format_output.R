format_output <- function(x,...){
  # Make a S3 class
  UseMethod('format_output',x)
}

format_output.lm <- function(x, refs=NULL, labs=NULL, pretty=T){
  tmp <- summary(x)$coef
  if(is.null(refs)){
    term <- attr(x$terms, 'term.labels')
  } else {
    term <- names(refs)
  }
  out <- data.frame(tmp[,c(1,2,4)])
  names(out) <- c('LOR','SE','pvalue')
  out$Variable <- get_variable(row.names(tmp), term)
  out$Level <- get_level(row.names(tmp), term)
  out$Level[out$Variable=='SEX'] <- 'Female'
  out <- out %>% mutate(Odds_Ratio = exp(LOR),
                        LCB = exp(LOR - 1.96 * SE), 
                        UCB = exp(LOR + 1.96*SE), 
                        Odds_Ratio = round(Odds_Ratio,2),
                        CI = paste0('(',round(LCB,2),
                                    ', ', round(UCB,2),')'),
                        pvalue = ifelse(pvalue < 0.001, '< 0.001',round(pvalue,3)))
  
  out <- out %>% select(Variable, Level, Odds_Ratio, CI, pvalue)
  
  out2 <- split(out, out$Variable)
  out2 <- out2[match(unique(out$Variable), names(out2))]
  
  if(!is.null(refs)){
    for (i in 1:length(refs)) {
      if (refs[i] != '' & names(refs)[i] %in% out$Variable) {
        vars <- names(refs)[i]
        out2[[vars]] <- rbind(c(
          Variable = vars,
          Level = refs[i],
          Odds_Ratio = 1,
          CI = '',
          pvalue = ''
        ),
        out2[[vars]])
        if(pretty) out2[[vars]][-1,'Variable'] <- ''
      }
    }
    out <- do.call(rbind, out2)
  }
  out$Level[out$Level=='()'] <- ''
  out <- out %>% mutate(Variable = labs[Variable],
                        Variable = ifelse(is.na(Variable),'',Variable),
                        Result = paste(Odds_Ratio, CI)) %>% 
    select(Variable, Level, Result)
  names(out) <- c('Predictor','Level','Result')
  out <- out[-1,]
  return(out)
}


format_output.crr <- function(crmodel, refs, labs, pretty=T) {
  tmp <- summary(crmodel)
  term <- names(refs)
  out <- data.frame(cbind(tmp$conf.int[, c(1, 3, 4)], tmp$coef[, 5]))
  names(out) <- c('Hazard_Ratio', 'LCB', 'UCB', 'Pvalue')
  out$Variable <- get_variable(row.names(tmp$conf.int), term)
  out$Level <- get_level(row.names(tmp$conf.int), term)
  out <- out %>% mutate(
    Level = ifelse(nchar(Level) < 2, '', Level),
    Level = ifelse(Variable == 'SEX', 'Female', Level),
    Level = ifelse(Variable == 'HISPANIC', '', Level),
    Level = ifelse(Variable=='RACE2', race_labels[Level],Level),
    Level = ifelse(Variable=='BMI', bmi_labels[Level], Level),
    Level = ifelse(Level=='Hispanic', Level, str_replace(Level, 'Hisp','')), # For hispanic groups
    pvalue = ifelse(Pvalue < 0.001, '< 0.001', round(Pvalue, 3)),
    Hazard_Ratio = round(Hazard_Ratio, 2),
    CI = paste0('(', round(LCB, 2), ', ', round(UCB, 2), ')')
  ) %>%
    select(Variable, Level, Hazard_Ratio, CI, pvalue)
  
  out2 <- split(out, out$Variable)
  out2 <- out2[match(unique(out$Variable), names(out2))]
  
  for (i in 1:length(refs)) {
    if (refs[i] != '' & names(refs)[i] %in% out$Variable) {
      vars <- names(refs)[i]
      out2[[vars]] <- rbind(c(
        Variable = vars,
        Level = refs[i],
        Hazard_Ratio = 1,
        CI = '',
        pvalue = ''
      ),
      out2[[vars]])
      #    if(pretty) out2[[vars]][-1,'Variable'] <- ''
    }
  }
  
  out <- do.call(rbind, out2)
  if(!is.null(labs)){
    out <- out %>% mutate(Variable = labs[Variable],
                          Variable = ifelse(is.na(Variable),'',Variable),
                          Result = paste(Hazard_Ratio, CI)) %>% 
      select(Variable, Level, Result)
  }
  names(out) <- c('Predictor','Level','Result')
  
  # Order races in correct order
  if('Race' %in% out$Predictor){
    ind1 <- which(out$Predictor=='Race')
    ind2 <- match(c('White','Black','Asian/Pacific Islander','AI/AN','Hispanic'), out$Level)
    out[ind1,] <- out[ind2,]
  }
  # Order hispanic subgroups
  if('Hispanic groups' %in% out$Predictor){
    ind1 <- which(out$Predictor=='Hispanic groups')
    ind2 <- match(c('Nonhispanic White','PR','US','Other','Unknown'), out$Level)
    out[ind1,] <- out[ind2,]
    out[ind1,'Level'] <- c('Non-hispanic white','Puerto Rican','Native-born Hispanic','Foreign-born Hispanic','Unknown nativity Hispanic')
  }
  # Order bmi levels
  ind1 <- which(out$Predictor == 'Body Mass Index (kg/m^2^)')
  ind2 = match(c('< 18.5','18.5-24.9','25-29.9','> 30'), out$Level)
  out[ind1,] = out[ind2,]
  out <- out %>% mutate(Level = str_replace(Level,'Paretenial','Peritoneal'))
  if(pretty){
    mult_rows = names(which(table(out$Predictor)>1))
    for(u in mult_rows){
      ind = which(out$Predictor==u)
      out$Predictor[ind[-1]] <- ''
    }
  }
  return(out)
}

format_output.coxph <- function(coxmodel, refs, labs, pretty=T){
  tmp <- summary(coxmodel)$conf.int
  if(is.null(refs)){
    term <- attr(coxmodel$terms, 'term.labels')
  } else {
    term <- names(refs)
  }
  out <- data.frame(tmp[,c(1,3,4)])
  names(out) <- c('HR','LCB','UCB')
  out$Variable <- get_variable(row.names(tmp), term)
  out$Level <- get_level(row.names(tmp), term)
  out <- out %>% mutate(
    Level = ifelse(nchar(Level) < 2, '', Level),
    Level = ifelse(Variable == 'SEX', 'Female', Level),
    Level = ifelse(Variable == 'HISPANIC', '', Level),
    Level = ifelse(Variable=='RACE2', race_labels[Level],Level),
    Level = ifelse(Variable=='BMI', bmi_labels[Level], Level),
    Level = ifelse(Level=='Hispanic', Level, str_replace(Level, 'Hisp','')), # For hispanic groups
    HR = round(HR, 2),
    CI = paste0('(', round(LCB, 2), ', ', round(UCB, 2), ')'),
    Result = paste0(HR,' ',CI)
  ) %>%
    select(Variable, Level, Result)
  
  # out <- out %>% mutate(HR = round(HR,2),
  #                       CI = paste0('(',round(LCB,2),
  #                                   ', ', round(UCB,2),')'))
  
  # out <- out %>% select(Variable, Level, HR, CI)
  
  out2 <- split(out, out$Variable)
  out2 <- out2[match(unique(out$Variable), names(out2))]
  
  if(!is.null(refs)){
    for (i in 1:length(refs)) {
      if (refs[i] != '' & names(refs)[i] %in% out$Variable) {
        vars <- names(refs)[i]
        out2[[vars]] <- rbind(c(
          Variable = vars,
          Level = refs[i],
          HR = 1,
          CI = ''
        ),
        out2[[vars]])
        # if(pretty) out2[[vars]][-1,'Variable'] <- ''
      }
    }
    out <- do.call(rbind, out2)
  }
  out <- out %>% mutate(Variable = labs[Variable],
                        Variable = ifelse(is.na(Variable),'',Variable))
  names(out) <- c('Predictor','Level','Result')
  # Order races in correct order
  ind1 <- which(out$Predictor=='Race')
  ind2 <- match(c('White','Black','Asian/Pacific Islander','AI/AN','Hispanic'), out$Level)
  out[ind1,] <- out[ind2,]
  # Order bmi levels
  ind1 <- which(out$Predictor == 'Body Mass Index (kg/m^2^)')
  ind2 = match(c('< 18.5','18.5-24.9','25-29.9','> 30'), out$Level)
  out[ind1,] = out[ind2,]
  out <- out %>% mutate(Level = str_replace(Level,'Paretenial','Peritoneal'))
  if(pretty){
    mult_rows = names(which(table(out$Predictor)>1))
    for(u in mult_rows){
      ind = which(out$Predictor==u)
      out$Predictor[ind[-1]] <- ''
    }
  }
  return(out)
  
  out <- out[-1,]
  return(out)
}
