#' Reloading utility functions into R
#'
#' This function creates a new environment in R, sources all the R script files
#' in a directory into that environment, and attaches that environment to the 
#' current namespace. This is a "poor man" packaging system before a formal package is written
#' 
#' This function is meant to load utility functions into R without cluttering the 
#' namespace. It is a "poor man's" packaging withouth formal packaging
#' @param d The directory where the utility functions reside. Defaults to the "lib"
#'   sub-directory of the current directory
#' @return Invisibly attaches an environment to the current workspace, named "fn"
#' @export
reload <- function(d = file.path(getwd(),'lib')){
  source(file.path(d,'packages.R'))
  if('fn' %in% search()) detach('fn')
  fn <- new.env()
  source(file.path(d, 'packages.R')) # Always load packages first
  for(f in dir(file.path(d,'R'), pattern='.R')){
    print(paste('Loading',f))
    source(file.path(d,'R',f), local=fn)
  }
  attach(fn)    
}
