#' @describeIn get_ get_ 
#' Get uPheno
#'
#' Get data from the \href{https://github.com/obophenotype/upheno}{
#' Unified Phenotype Ontology (uPheno)}.
#'
#' @param file Can be one of the following:
#' \describe{
#' \item{"ontology"}{Creates an \link[simona]{ontology_DAG} R object by
#'  importing the OBO file directly from the official
#'  \href{https://github.com/obophenotype/upheno}{uPheno GitHub repository}.}
#' \item{"bestmatches"}{Returns a merged table with the best matches between
#'  human and non-human homologous phenotypes (from multiple species).
#'  Distributed by the official
#'  \href{https://github.com/obophenotype/upheno/tree/master/mappings}{
#'  uPheno GitHub repository}.}
#' \item{"upheno_mapping"}{Return a merged table with matches between human
#' and non-human homologous phenotypes (from multiple species).
#' Distributed by the
#'  \href{https://data.monarchinitiative.org/upheno2/current/upheno-release/all/index.html}{
#'  Monarch Initiative server}.}
#' }
#' @returns \link[simona]{ontology_DAG} or \link[data.table]{data.table}.
#'
#' @export
#' @examples
#' upheno <- get_upheno()
get_upheno <- function(file=c("ontology",
                              "bestmatches",
                              "upheno_mapping")){
  file <- file[1]
  if(file=="ontology"){
    ont <- get_ontology("upheno") 
    return(ont)
  }
  if(file=="bestmatches"){
    #### Fuzzy query ####
    ## Only between HPO and 3 ontologies
    base_url <- 
      "https://raw.githubusercontent.com/obophenotype/upheno/master/mappings/"
    URLs <- paste0(base_url,
                   c("hp-to-zp-bestmatches.tsv",
                     "hp-to-mp-bestmatches.tsv",
                     "hp-to-wbphenotype-bestmatches.tsv"))
    pheno_map <- lapply(URLs, function(x){
      data.table::fread(x)
    }) |> `names<-`(gsub("-bestmatches.tsv","",basename(URLs))) |>
      data.table::rbindlist(idcol = "map", fill = TRUE) |>
      `names<-`(c("map",
                  "subject","subject_label",
                  "object","object_label",
                  "equivalence_score","subclass_score"))
  }
  if (file=="upheno_mapping"){
    subject <- object <- p1 <- p2 <- NULL;
    pheno_map <- get_monarch(queries = "phenotype_phenotype")
    pheno_map[,subject:=gsub("_",":",basename(p1))
              ][,object:=gsub("_",":",basename(p2))] 
    data.table::setkeyv(pheno_map,"subject")
  }
  
  add_db(dat = pheno_map, 
         input_col = "subject")
  add_db(dat = pheno_map, 
         input_col = "object") 
  return(pheno_map)
}
