#' @keywords internal
standardise_ontology_name <- function(name){
  
  name <- tolower(name)
  if(name=="hpo") return("hp")
  if(name %in% c("cl","cellontology","cell-ontology")) return("cl")
  
  return(name)
}