#' @describeIn cache_ cache_
#' Cache directory
#' 
#' Provides the path to the package-wide cache directory.
#' @inheritParams tools::R_user_dir
#' @returns Cache directory path.
#' 
#' @export
#' @examples
#' save_dir <- cache_dir
cache_dir <- function(package="KGExplorer",
                      subdir=NULL){
  
  dir <- tools::R_user_dir(
    package = package,
    which = "cache"
  )
  if(!is.null(subdir)){
    dir <- file.path(dir,subdir)
  }
  dir.create(dir,showWarnings = FALSE, recursive = TRUE)
  return(dir)
}
