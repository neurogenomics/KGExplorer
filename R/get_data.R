#' @describeIn utils_ utils_
#' Get remote data
#'
#' Download remotely stored data via \link[piggyback]{pb_download}. 
#' @inheritParams get_ 
#' @param add_version Add the release version
#'  to the returned object's \link[base]{attributes}
#' @returns Path to downloaded file or the object itself (when ".rds" format).
#'
#' @keywords internal
#' @inheritParams piggyback::pb_download
get_data <- function(file,
                     tag = "latest",
                     repo = "neurogenomics/KGExplorer",
                     save_dir = cache_dir(),
                     add_version = FALSE,
                     overwrite = TRUE
                     ){
  requireNamespace("piggyback")
  Sys.setenv("piggyback_cache_duration" = 10)

  v <- NULL
  tmp <- file.path(save_dir, file)
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  if(!file.exists(tmp) ||
     isTRUE(overwrite)){
    piggyback::pb_download(
      file = file,
      tag = tag,
      dest = save_dir,
      repo = repo,
      overwrite = overwrite)
    #### Get exact release version name ####
    if(isTRUE(add_version)){
      releases <- piggyback::pb_releases(repo = repo) |>
        data.table::data.table()
      if(tag=="latest"){
        v <- releases$tag_name[1]
      } else{
        v <- tag
      }
    }
  }
  #### Read/return ####
  if(grepl("\\.rds$",tmp,ignore.case = TRUE)){
    obj <- readRDS(tmp)
    if(!is.null(v)) attr(obj,"version") <- v
    return(obj)
  } else{
    if(!is.null(v)) attr(tmp,"version") <- v
    return(tmp)
  }
}


