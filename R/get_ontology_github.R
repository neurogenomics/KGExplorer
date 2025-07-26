#' Get ontology from GitHub and import it via \link[simona]{import_ontology}.
#' @inheritParams get_
#' @inheritDotParams simona::import_ontology
#' @inheritParams piggyback::pb_download_url  
#' @inheritParams get_ontology
#' @returns \link[simona]{ontology_DAG}
#'
#' @keywords internal
get_ontology_github <- function(name,
                                repo,
                                filetype=".obo",
                                file=paste0(name,filetype),  
                                tag="latest",
                                
                                add_metadata=TRUE,
                                add_n_edges=TRUE,
                                add_ontology_levels=TRUE,
                                lvl=2,
                                
                                save_dir=cache_dir(subdir = "ontologies"),
                                force_new=FALSE,
                                ...){
  
  messager("Importing ontology via GitHub.")
  #### Get latest release/tag/version ####  
  if(is.null(tag) || tag == "latest"){
    requireNamespace("piggyback") 
    messager("Identifying latest release for:",repo)
    releases <- piggyback::pb_releases(repo = repo)
    tag <- releases$tag_name[1]
  } 
  
  save_path <- .get_ontology_savepath(save_dir=save_dir, 
                                      path=file, 
                                      version=tag)
  
  if(!file.exists(save_path) || isTRUE(force_new)){
    requireNamespace("piggyback") 
    URL <- piggyback::pb_download_url(file = file, 
                                      repo = repo,
                                      tag = tag)
    
    messager("Preparing ontology_index object from:",URL)
    ont <- simona::import_ontology(file=URL,
                                   ...) 
    
    #### Add metadata #### 
    if(isTRUE(add_metadata)) {
      ont <- add_ontology_metadata(ont,
                                   add_n_edges=add_n_edges,
                                   lvl=lvl, 
                                   add_ontology_levels=add_ontology_levels)
    }
    
    #### Cache file ####
    if(!is.null(save_dir)){
      dir.create(save_dir, recursive=TRUE, showWarnings = FALSE)
      messager("Caching file -->",save_path)
      saveRDS(ont,save_path)
    }
    
  } else {
    #### Import cached file ####
    messager("Importing cached file:",save_path)
    ont <- readRDS(save_path)
  }
  return(ont)
}
