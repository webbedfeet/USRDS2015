transform_indx <- function(x){
  case_when(
    x=='stroke_primary'~ 'Primary stroke',
    x=='stroke_compl' ~ 'Stroke with complications',
    x == 'dement' ~ 'Dementia',
    x == 'thrive' ~ 'Failure to thrive',
    x == 'LuCa' ~ 'Lung cancer',
    x == 'MetsCa' ~ 'Metastatic cancer'
  )
}