#' @describeIn filter_ filter_
#' Filter ontology
#'
#' Filter ontology by terms.
#' @inheritDotParams simona::dag_filter
#' @export
#' @examples
#' ont <- get_ontology("hp")
#' ont2 <- filter_ontology(ont,terms=c("HP:0000001","HP:0000002"))
#' ont3 <- filter_ontology(ont,terms=100)
filter_ontology <- function(ont,
                            terms=NULL,
                            remove_terms=NULL,
                            keep_descendants=NULL,
                            remove_descendants=NULL,
                            include_self = TRUE,
                            use_simona=FALSE,
                            ...){
  #### Check remove_terms #### 
  terms <- terms[!terms %in% remove_terms]
  #### Use simona ####
  if(isTRUE(use_simona)){
    ont <- simona::dag_filter(ont, terms=terms, ...)
    return(ont)
  }
  #### keep_descendants ####
  if(!is.null(keep_descendants)){
    keep_descendants <- map_ontology_terms(ont = ont,
                                           terms = keep_descendants,
                                           to = "id") |> stats::na.omit()
    if(length(keep_descendants)>0){
      messager("Keeping descendants of",length(keep_descendants),"term(s).")
      ont <- simona::dag_filter(ont, 
                                root=as.character(keep_descendants),
                                ...)
      messager(formatC(ont@n_terms,big.mark = ","),
               "terms remain after filtering.")
    } else {
      messager("keep_descendants: No descendants found.")
    } 
  }
  #### remove_descendants ####
  if(!is.null(remove_descendants)){
    remove_descendants <- map_ontology_terms(ont = ont,
                                             terms = remove_descendants,
                                             to = "id") |> stats::na.omit()
    if(length(remove_descendants)>0){
      messager("Removing descendants of",length(remove_descendants),"term(s).")
      remove_descendants <- simona::dag_offspring(dag = ont,
                                                  include_self = include_self,
                                                  term = remove_descendants)
      keep_terms <- ont@terms[!ont@terms %in% remove_descendants]
      ont <- simona::dag_filter(ont, 
                                terms=keep_terms, 
                                ...)
      messager(formatC(ont@n_terms,big.mark = ","),
               "terms remain after filtering.")
    } else {
      messager("remove_descendants: No descendants found.")
    }
  }
  #### Use custom filtering methods ####
  if(!is.null(terms)){
    terms <- map_ontology_terms(ont = ont,
                                terms = terms,
                                to = "id") |> stats::na.omit()
    ## Characters 
    if(is.character(terms)){
      terms <- terms[simona::dag_has_terms(dag=ont, terms = unique(terms))]
      if(length(terms)==0) {
        stopper("None of the supplied terms found in the ontology.")
      } 
      ont <- ont[,terms]
      
    } else if (is.numeric(terms)){
      messager("Randomly sampling",terms,"term(s).")
      if(terms>length(ont@terms)){
        messager(
          "Number of terms requested exceeds number of terms in the ontology.",
          "Returning original ontology object without filtering.")
        return(ont) 
      } 
      if(terms==0) stopper("Terms must be >0 if numeric.")
      term_ids <- sample(ont@terms,terms, replace = FALSE)
      ont <- ont[,term_ids]
    }  
  }
  return(ont)
}