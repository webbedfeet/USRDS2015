#' table_results
#' 
#' This version creates a publishable results table from a Cox regression 
#' run using the `survival::coxph` function. Features include:
#' 
#' + Adding the reference class for each categorical predictor
#' + Choosing to report 95% confidence intervals or p-values (TODO)
#' + 
#'
#' @param mod A `coxph` object
#' @param lbls An optional named vector providing readable labels for each predictor variable
#' @param tidy A boolean (default=F) determining if the output is a tidy tibble or a untidy printable table. Use `tidy = T` if you want to further manipulate the table, or add other results to this table using joins, for example
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(survival)
#' 
table_results.coxph <- function(mod, lbls = NULL, tidy = F){
  require(survival)
  require(dplyr)
  require(broom)
  out <- tidy(mod) %>% select(term, estimate, conf.low, conf.high) %>% 
    mutate_at(vars(-term), exp) %>% 
    mutate(res = as.character(
      glue::glue('{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high, 2)})'))) %>% 
    select(term, res)
  out <- out %>% 
    mutate(variables = str_extract(term, paste(attr(mod$terms, 'term.labels'), collapse='|'))) %>% 
    mutate(levels = str_replace(term, variables, '')) %>% 
    select(variables, levels, res)
  baselevels <- data.frame(levels = unlist(map(mod$xlevels, `[`, 1))) %>% 
    rownames_to_column(var = 'variables') %>% 
    mutate(res = '1')
  if(nrow(baselevels) > 0){ # Do we have categorical predictors
    for(i in 1:nrow(baselevels)){
      out <- out %>% add_row(variables = baselevels[i,1], levels = baselevels[i,2],
                             res = '1.00 (ref)', .before = which.max(out$variables == baselevels[i,1]))
    }
    if (!tidy){
      for (i in 1:nrow(baselevels)){
        out <- out %>% add_row(variables = baselevels[i,1], levels = '', res = '',
                               .before = which.max(out$variables == baselevels[i,1]))
      }
    }
  }
  if(!is.null(lbls)){
    out <- out %>% mutate(variables = lbls[variables])
  }
  if(!tidy){
    out <- out %>% clean_cols(variables) %>% 
      mutate(levels = paste0('    ', levels)) %>% 
      unite('Variables', variables, levels, sep = '')
  }
  if(tidy){
    if(all(out$levels == '')) out <- select(out, -levels)
    out <- out %>% rename(Variable = 'variables')
  }
  out <- out %>% rename('HR (95% CI)' = 'res')
  return(out)
}
