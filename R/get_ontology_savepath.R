.get_ontology_savepath <- function(save_dir, 
                                   path, 
                                   ext_new="rds",
                                   version=NULL) {
  ext_old <- .file_ext(path)
  if (is.null(ext_new)){
    ext_new <- ext_old
  }
  if(ext_old!=""){
    path_new <- gsub(paste0(".",ext_old,"$"),"",path)
  } else {
    path_new <- path
  }
  save_path <- file.path(save_dir, 
                         paste0(path_new,
                                if(!is.null(version))"_",version,
                                ".",ext_new))
  return(save_path)
}