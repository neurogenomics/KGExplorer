#' @describeIn get_ get_
#' Get Monarch
#'
#' Get key datasets from the
#' \href{https://monarchinitiative.org/}{Monarch Initiative}
#' \href{https://data.monarchinitiative.org}{server}.
#' See
#' \href{https://data.monarchinitiative.org/latest/tsv/all_associations/}{here}
#'  for all associations data, specifically. 
#' @source \href{https://github.com/charlieccarey/monarchr}{
#' monarchr R package (abandoned project?)} 
#' @inheritParams get_monarch_files
#' @inheritDotParams data.table::fread
#' @returns \link[data.table]{data.table}
#'
#' @export
#' @examples
#' dat <- get_monarch(maps=list(c("causal_gene","disease")))
get_monarch <- function(queries=NULL,
                        maps=NULL,
                        domain="https://data.monarchinitiative.org",
                        subdir="monarch-kg/latest/tsv/all_associations/",
                        rbind=FALSE,
                        save_dir=cache_dir()
                        ){
  files <- get_monarch_files(domain=domain,
                             subdir=subdir,
                             maps=maps,
                             queries=queries) 
  #### Download and prepare maps ####
  messager("Importing",nrow(files),"Monarch files.")
  dat_list <- lapply(stats::setNames(seq(nrow(files)),
                                     files$name), function(i){ 
    messager("-",paste0(i,"/",nrow(files),":"),
             files[i,]$name) 
    tryCatch({
      data.table::fread(files[i,]$url, 
                        tmpdir = save_dir)
    }, error=function(e){messager(e);NULL})
  }) 
  ### Bind
  if(isTRUE(rbind)){
    return(
      data.table::rbindlist(dat_list,idcol = "file")
    )
  } else { 
    return(dat_list)
  } 
}
