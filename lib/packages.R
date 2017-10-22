## Packages to load
pkgs <- c(
  'tidyverse',
  'stringr',
  'survival',
  'broom',
  'DBI',
  'RSQLite',
  'MonetDBLite',
  'reshape2',
  'cowplot',
  'cmprsk',
  'rmarkdown'
)
for (p in pkgs) {
  if (!(p %in% installed.packages()[,1])) {
    install.packages(p, repos = 'http://cran.rstudio.com')
  }
  library(p, character.only = TRUE, quietly = TRUE)
}
