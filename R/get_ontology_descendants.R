#' Get ontology descendants
#' 
#' Get the descendants of a set of ontology terms.
#' @inheritParams get_ 
#' @returns A named list of descendants, where the names are the input terms.
#' @param ... Additional arguments passed to  \code{simona::dag_offspring}.
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' d <- get_ontology_descendants(ont, c("HP:0000001","HP:0000002"))
get_ontology_descendants <- function(ont,
                                     terms,
                                     include_self = TRUE,
                                     ...){
  lapply(terms, function(x){
    # message(x)
    xt <- map_ontology_terms(ont = ont,
                             terms = x,
                             to = 'id')
    if(all(is.na(xt))) {
      messager("WARNING: The term",x,"was not found in the ontology.")
      return(NULL)
    }
    simona::dag_offspring(ont,
                          include_self = include_self,
                          term=xt,
                          ...)
  })
}
