#' @describeIn map_ map_
#' Map ontology terms to an alternative name.
#'
#' Harmonise a mixed vector of term names (e.g. "Focal motor seizure")
#' and term IDs (e.g. c("HP:0000002","HP:0000003")).
#' @param terms A character vector of term names and/or term IDs.
#' @param keep_order Return a named list of the same length and order
#' as \code{terms}.
#' If \code{FALSE}, return a named list of only the unique \code{terms},
#' sometimes in a different order.
#' @param invert Invert the keys/values of the dictionary,
#' such that the key becomes the values (and vice versa).
#' @param ignore_case Ignore case when mapping terms.
#' @param ignore_char A character vector of characters to ignore when 
#' mapping terms.
#' @param verbose Print messages.
#' @returns Character vector.
#'
#' @export
#' @examples
#' ont <- get_ontology("hp")
#' terms <- c("Focal motor seizure",
#'             "Focal MotoR SEIzure",
#'             "Focal-motor,/seizure.",
#'             "HP:0000002","HP:0000003")
#' term_names <- map_ontology_terms(ont=ont, terms=terms)
#' term_ids <- map_ontology_terms(ont=ont, terms=terms, to="id")
map_ontology_terms <- function(ont,
                               terms = NULL,
                               to=c("name","id","short_id"),
                               keep_order = TRUE,
                               invert = FALSE,
                               ignore_case=TRUE,
                               ignore_char=c("-","/",",","\\."),
                               verbose=1){
  from <- input <- NULL;
  
  to <- match.arg(to)
  if(is.null(terms)) {
    terms <- ont@terms
  } else {
    terms <- as.character(terms)
  }
  terms_og <- terms
  
  if(to=="id") to <- "short_id"
  
  # terms <- unique(terms)
  # new_to_old <- stats::setNames(unique(terms_og),terms)
  #### to IDs ###
  if(to %in% c("id","short_id")){
    messager("Translating ontology terms to ids.",v=verbose)
    map <- get_ontology_dict(ont,
                             from="name",
                             to=to,
                             as_datatable=TRUE,
                             include_self=TRUE)
  } else {
    #### to names ###
    messager("Translating ontology terms to names.",v=verbose)
    map <- get_ontology_dict(ont,
                             from="short_id",
                             to=to,
                             as_datatable=TRUE,
                             include_self=TRUE)
  } 
  if(isTRUE(ignore_case)){ 
    map <- rbind(
      map,
      data.table::copy(map)[,from:=tolower(from)]
    ) |> unique()
    terms <- tolower(terms)
  } 
  if(length(ignore_char)>0){
    rm_char <- function(x, ignore_char){
      x <- gsub(paste0(paste0("[",ignore_char,"]"),collapse="|")," ",x)
      x <- gsub("\\s+"," ",x)
      trimws(x)
    }
    map <- rbind(
      map,
      data.table::copy(map)[,from:=rm_char(from, ignore_char)]
    ) |> unique()
    terms <- rm_char(terms, ignore_char)
  }
  # map og terms onto new terms
  old_to_new <- stats::setNames(terms,terms_og) 
  data.table::setkeyv(map,"from")
  map <- map[unname(old_to_new),][,input:=names(old_to_new)] 
  map_tmp <- unique(map[,c("input","to")]) 
  data.table::setkeyv(map_tmp,"input")
  #### Return ####
  if(isFALSE(keep_order)){
    messager("+ Returning a dictionary of terms",
             "(different order as input).",v=verbose>1) 
    dict <- stats::setNames(map_tmp$to,
                            map_tmp$input)
    #### Invert ####
    if(isTRUE(invert)){
      dict <- invert_dict(dict)
    }
    return(dict)
  } else {
    messager("+ Returning a vector of terms",
             "(same order as input).",v=verbose>1) 
    return(
      stats::setNames(map_tmp[terms_og]$to,
                      terms_og)
      )
  }
}
