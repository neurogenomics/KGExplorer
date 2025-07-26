get_ontology_url <- function(URL,
                             save_dir=cache_dir(subdir = "ontologies"),
                             save_name=NULL,
                             version=NULL,
                             import_func=simona::import_ontology,
                             
                             add_metadata=TRUE,
                             add_n_edges=TRUE,
                             add_ontology_levels=TRUE,
                             lvl=2,
                            
                             force_new=FALSE,
                             ...){
  
  # Get the save path of the RDS object
  if(is.null(save_name)){
    save_name <- basename(URL)
  }
  save_path <- .get_ontology_savepath(save_dir=save_dir, 
                                      path=save_name, 
                                      version=version)
  
  #### Import cached file ####
  if(file.exists(save_path) && isFALSE(force_new)){
    messager("Importing cached file:",save_path)
    ont <- readRDS(save_path)
    return(ont)
  }
  
  #### Get new file ####
  
  ## import_owl works better for OWL files
  if(is.null(import_func)){
    if(grepl("\\.owl$",URL, ignore.case = TRUE)){
      import_func <- simona::import_owl
    } else {
      import_func <- simona::import_ontology
    } 
  } 
  ont <- import_func(URL, ...)
  
  #### Add metadata #### 
  if(isTRUE(add_metadata)) {
    ont <- add_ontology_metadata(ont,
                                 add_n_edges=add_n_edges,
                                 lvl=lvl, 
                                 add_ontology_levels=add_ontology_levels)
  }
  
  #### Cache RDS object ####
  if(!is.null(save_dir)){
    dir.create(save_dir, recursive=TRUE, showWarnings = FALSE)
    messager("Caching file -->",save_path)
    saveRDS(ont, save_path)   
  }
  return(ont)
}