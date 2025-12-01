#' @describeIn get_ get_
#' Get a complete up=to-date list of ontologies available via the
#' \href{https://www.ebi.ac.uk/ols4}{EBML-EBI Ontology Lookup Service} API.
#' @param ol An \code{rols::olsOntologies} object.
#' @importFrom utils packageVersion
#' @export
get_ols_options <- function(ol = NULL){
  # rols annoyingly changed their API without any deprecation warnings.... 
  if(is.null(ol)){
    ol <- get_ols_ontologies()
  }
 
  sort(rols::olsNamespace(ol))
}

get_ols_ontologies <- function(){
  if (utils::packageVersion("rols")>="2.99.5"){
    fun_name <- "olsOntologies" 
  } else{
    fun_name <- "Ontologies" 
  }
  ol <- utils::getFromNamespace(ns = "rols", fun_name) 
  return(ol)
}