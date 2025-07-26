#' @describeIn get_ get_ontology
#' Get ontology
#' 
#' Import an up-to-date ontology directly from from the creators or via the
#' \href{https://www.ebi.ac.uk/ols4}{EBML-EBI Ontology Lookup Service} API.
#' @param name
#' \itemize{
#' \item{<...>}{Any ontology name from \link{get_ols_options}}
#' \item{"mondo"}{
#' Import the \href{https://mondo.monarchinitiative.org/}{Mondo} ontology.
#' \href{https://github.com/monarch-initiative/mondo/release}{
#' GitHub.}}.
#' \item{"hpo"}{
#' Import the \href{https://hpo.jax.org/app/}{Human Phenotype Ontology}.
#' \href{https://github.com/obophenotype/human-phenotype-ontology/release}{
#' GitHub.}}
#' }
#' @param method Whether to import ontology via the \code{rols} package or
#' via the \link{get_ontology_github}/link{get_ontology_url} functions. 
#' @param add_metadata Add metadata to the resulting ontology object.
#' @param filetype File type to search for.
#' @param import_func Function to import the ontology with. 
#' If \code{NULL}, automatically tries to choose the correct function.
#' @inheritParams piggyback::pb_download_url
#' @inheritDotParams get_ontology_github
#' @returns \link[simona]{ontology_DAG}
#' 
#' @export
#' @examples
#' mondo <- get_ontology(name="mondo")
#' \dontrun{
#'   hp <- get_ontology(name="hp")
#'   upheno <- get_ontology(name="upheno")
#' }
get_ontology <- function(name=c("mondo",
                                "hp",
                                "upheno",
                                "uberon",
                                "cl"),
                         ## Safer to use OBO files (via GitHub) 
                         ## rather than OWL files (via rols)
                         method=c("github",
                                  "rols"), 
                         filetype=".obo",
                         import_func=NULL,
                         terms=NULL,
                         
                         add_metadata=TRUE,
                         lvl=2,
                         add_n_edges=TRUE,
                         add_ontology_levels=TRUE,
                         save_dir=cache_dir(subdir="ontologies"),
                         tag=NULL,
                         
                         force_new=FALSE,
                         ...){ 
  name <- name[1]
  method <- method[1]
  method <- match.arg(method) 
  
  name <- standardise_ontology_name(name)
  
  # Make a sub-subdir for each method
  save_dir <- file.path(save_dir, method)
  
  # Check if cached file exists
  if (isFALSE(force_new)){
    cached_files <- list.files(save_dir, 
                               pattern = paste0(name,"_",tag,".*\\.rds"), 
                               full.names = TRUE)
    if (length(cached_files) > 0) {
      f <- cached_files[1]
      messager("Using cached ontology file",
               paste0("(1/",length(cached_files),"):\n"),
               f)
      ont <- readRDS(f)
      #### Subset ontology ####
      ont <- filter_ontology(ont = ont, 
                             terms = terms)
      return(ont)
    }
  }
  
  ## Get new file 
  ol <- rols::Ontologies()
  #### Assess options available via github and/or rols ####
  rols_opts <- get_ols_options(ol=ol)
  if(method=="rols" && 
     !name %in% rols_opts){
    messager("Ontology not found via 'rols.' Trying method='github'.")
    method <- "github"
  }
  if(method=="github" &&
     !name %in% c("mondo",
                  "hp",
                  "cl",
                  "upheno",
                  "uberon")){
    if(name %in% rols_opts){
      messager("Ontology not found via 'github.' Using method='rols'.")
      method <- "rols"
    } else {
      stop("Ontology not found. Please check the name.")
    }
  }
  #### via EMBL-EBI Ontology Lookup Service ####
  if(method=="rols"){ 
    
    if(!is.null(tag)) messager("`tag` is ignored when `method == 'rols'`")
    
    ol_ont <- ol[[name]]
    version <- sub("([0-9]+-[0-9]+-[0-9]+).*", "\\1", ol_ont@updated) 
    messager("Importing ontology via `rols`.")
    ont <- get_ontology_url(
      URL = ol_ont@config$fileLocation,
      version = version,
      import_func=import_func,
      force_new = force_new, 
      save_dir = save_dir, 
      save_name = ol_ont@ontologyId,
      
      add_metadata=add_metadata,
      add_n_edges=add_n_edges,
      lvl=lvl, 
      add_ontology_levels=add_ontology_levels,
      ...) 
    
  #### Via manually coded functions ####
  } else if(method=="github"){
    if(name=="mondo"){
      ont <- get_ontology_github(name=name, 
                                 repo="monarch-initiative/mondo",
                                 filetype=filetype,
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 tag=tag,
                                 
                                 add_metadata=add_metadata,
                                 add_n_edges=add_n_edges,
                                 lvl=lvl, 
                                 add_ontology_levels=add_ontology_levels,
                                 ...)
      
    } else if(name=="hp"){
      ont <- get_ontology_github(name=name, 
                                 repo="obophenotype/human-phenotype-ontology",
                                 filetype=filetype,
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 tag=tag,
                                 
                                 add_metadata=add_metadata,
                                 add_n_edges=add_n_edges,
                                 lvl=lvl, 
                                 add_ontology_levels=add_ontology_levels,
                                 ...)
      
    } else if (name=="cl"){
      ont <- get_ontology_github(name=name, 
                                 repo="obophenotype/cell-ontology",
                                 filetype=filetype,
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 tag=tag,
                                 
                                 add_metadata=add_metadata,
                                 add_n_edges=add_n_edges,
                                 lvl=lvl, 
                                 add_ontology_levels=add_ontology_levels,
                                 ...)
      
    } else if(name=="upheno"){
      ont <- get_ontology_url(URL = 
        # "https://github.com/obophenotype/upheno/raw/master/upheno.owl",
        "https://purl.obolibrary.org/obo/upheno/v2/upheno.owl",
        import_func = simona::import_owl,
        force_new = force_new, 
        save_dir = save_dir,
        save_name = "upheno",
        
        add_metadata=add_metadata,
        add_n_edges=add_n_edges,
        lvl=lvl, 
        add_ontology_levels=add_ontology_levels,
        ...) 
      
    } else if (name=="uberon"){
      ont <- get_ontology_github(name=name, 
                                 repo="obophenotype/uberon",
                                 filetype=filetype,
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 tag=tag,
                                 
                                 add_metadata=add_metadata,
                                 add_n_edges=add_n_edges,
                                 lvl=lvl, 
                                 add_ontology_levels=add_ontology_levels,
                                 ...)
    }  
  } 
  
  
  #### Subset ontology ####
  ont <- filter_ontology(ont = ont, 
                         terms = terms)
  #### Return ####
  return(ont)
}