#' @describeIn get_ get_
#' Get ontology level for ontology terms
#'
#' For a given set of HPO terms, get their level
#' within the hierarchically organised ontology.
#' Ontology level can be computed either absolute mode (\code{absolute=TRUE})
#' where the entire ontology is considered when assigning levels, or
#' relative mode (\code{absolute=FALSE}) where only a subset of the ontology
#' that is connected to a given term is considered when assigning levels.
#' Relative mode can be helpful when trying to make plot where nodes are
#' scaled to the ontology level.
#' @param method Compute ontology levels using:
#' \describe{
#'  \item{"height" (default)}{\link[simona]{dag_height}.}
#'  \item{"depth"}{\link[simona]{dag_depth}.}
#' }
#' @param absolute Make the levels absolute in the sense that they consider
#'  the entire ontology (\code{TRUE}).
#'  Otherwise, levels will be relative to only the terms that are in
#'   the provided subset of \code{terms} AND are directly adjacent (connected)
#'   to a given cluster of terms (\code{FALSE}).
#' @param reverse If \code{TRUE}, ontology
#' level numbers with be revered such that the level of the parent terms
#' are larger than the child terms.
#' @inheritParams filter_
#' @returns A named vector of relative ontology level,
#' where names are ontology term IDs and
#' value is relative ontology level.
#'
#' @export
#' @importFrom simona LCA_depth
#' @examples
#' ont <- get_ontology("hp")
#' terms <- ont@terms[1:10]
#' lvls <- get_ontology_levels(ont, terms = terms)
#' lvls_rel <- get_ontology_levels(ont, terms = terms, absolute=FALSE)
get_ontology_levels <- function(ont,
                                terms = NULL,
                                remove_terms = TRUE,
                                method=c("depth","height"),
                                absolute = TRUE,
                                reverse = FALSE) {
  #### Check inputs ####
  if(!methods::is(ont,"ontology_DAG")){
    stopper("Input must be an ontology object.")
  }
  method <- match.arg(method)
  if(is.null(terms)){
    terms <- ont@terms
  }
  terms <- unique(terms)
  messager("Getting",if(isTRUE(absolute)) "absolute" else "relative", 
           "ontology level for",
           formatC(length(terms),big.mark = ","),"IDs.")
  #### Get term depth #### 
  if(isFALSE(absolute)){
    # lvls <- simona::LCA_depth(ont, 
    #                        terms=terms) |> diag()
    ont <- filter_ontology(ont,
                           terms=terms,
                           remove_terms=remove_terms)
  } 
  if(method=="height"){
    lvls <- simona::dag_height(ont, 
                               terms=terms,
                               use_cache=FALSE)
  } else if(method=="depth"){
    lvls <- simona::dag_depth(ont,
                              terms=terms,
                              use_cache=FALSE)
  } 
  #### Reverse levels ####
  if(isTRUE(reverse)){
    lvls <- max(lvls) - lvls
  }
  return(lvls)
}
