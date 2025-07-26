#' @describeIn get_ get_
#' Get version
#'
#' For a given ontology, extract the precise version of the Release that the 
#' data object was built from. For Human Phenotype Ontology specifically,
#'  all Releases can be found at the official
#' \href{https://github.com/obophenotype/human-phenotype-ontology/releases}{
#' HPO GitHub Releases page}.
#' @param obj An object.
#' @param verbose Print messages.
#' @param return_version Return the version as a character string.
#' @returns Data object release version a character string.
#'
#' @export
#' @examples
#' obj <- get_ontology("hp")
#' get_version(obj=obj)
get_version <- function(obj,
                        return_version = FALSE,
                        verbose = TRUE){
  
  is_date <- function(x){
    grepl("[0-9]+-[0-9]+-[0-9]",x)
  }
  
  ## Extract
  if(methods::is(obj,"ontology_index")){
    x <- grep("data-version:",attr(obj,"version"),value=TRUE)
    v <- paste0("v",
                trimws(gsub("data-version:|hp/releases/|/hp-base.owl","",x))
                )
  } else if(methods::is(obj,"ontology_DAG")){
    v <- basename(strsplit(obj@source,",")[[1]][2])
    if (!is_date(v)){
      v <- basename(dirname(strsplit(obj@source,",")[[1]][2]))
    }
  } else {
    v <- attr(obj,"version")
  }
  ## Print
  messager("+ Version:",v,v=verbose)
  ## Return
  if(isTRUE(return_version))  return(v)
}
