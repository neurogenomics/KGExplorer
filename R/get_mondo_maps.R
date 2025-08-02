#' @describeIn get_ get_
#' Get Mondo ID maps
#' 
#' Get mappings between Mondo IDs and IDs in other databases/ontologies.
#' All mappings stored on the official
#'\href{https://github.com/monarch-initiative/mondo/tree/master/src/ontology/mappings}{
#'Mondo GitHub}.
#' @param top_by Grouping columns when selecting \code{top_n} rows per grouping.
#' Can be a character vector of one or more column names. 
#' @returns \link[data.table]{data.table} of mappings.
#' 
#' @export
#' @examples
#' map <- get_mondo_maps("default") 
get_mondo_maps <- function(map_types=c("default",
                                       "broadmatch",
                                       "closematch",
                                       "exactmatch",
                                       "hasdbxref",
                                       "narrowmatch",
                                       "relatedmatch"),
                           map_to = NULL,
                           map_type_order=c("default",
                                            "exactmatch",
                                            "closematch",
                                            "narrowmatch",
                                            "broadmatch",
                                            "relatedmatch",
                                            "hasdbxref"),
                           top_n=NULL,
                           top_by=c("subject","object"),
                           save_dir=cache_dir()
                           ){ 
  save_path <- subject_label <- object_label <- map_type <- label <- file <-
    predicate <- NULL;
  
  if(length(map_types)==1 &&
     map_types=="default"){ 
    
    save_path <- file.path(save_dir,"mondo.sssom.tsv")
    if(!file.exists(save_path)){
      dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
      utils::download.file(
        url =  paste0(
          "https://github.com/monarch-initiative/mondo/raw/master/",
          "src/ontology/mappings/mondo.sssom.tsv"),
        destfile = save_path)
    }
    
    map <- data.table::fread(save_path,
                             skip = "subject",
                             tmpdir = save_dir)
    data.table::setnames(map,
                         gsub("_id$","",names(map)))
    map[,file:=basename(save_path)]
    add_db(dat=map,
           input_col="subject",
           output_col="subject_db")
    add_db(dat=map,
           input_col="object",
           output_col="object_db")
  } else {
    files <- get_mondo_maps_files(save_dir = save_dir,
                                  map_types = map_types,
                                  map_to = map_to)
    #### Import maps #####
    map <- lapply(stats::setNames(files$link_raw,
                                  basename(files$link_raw)),
                  function(x){
                    
      save_path <- file.path(save_dir, basename(x))
      if(!file.exists(save_path)){ 
        dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
        utils::download.file(url = x,
                             destfile = save_path)
      }
      data.table::fread(save_path,
                        skip="subject_id",
                        tmpdir = save_dir)
    }) |> data.table::rbindlist(fill = TRUE, idcol = "file")
    data.table::setnames(map,
                         gsub("_id$","",names(map)))
    add_db(dat=map,
           input_col="object",
           output_col="object_db")
  }
  #### Sort mappings by confidence #### 
  map[,map_type:=data.table::tstrsplit(predicate,keep = 2,split=":")]
  map[,map_type:=factor(tolower(map_type),
                        levels = map_type_order, ordered = TRUE)]
  data.table::setorderv(map,cols = "map_type")
  #### Select the top mapping per ID ####
  if(is.numeric(top_n)){
    n1 <- nrow(map)
    map <- map[,utils::head(.SD, top_n), by=top_by]
    messager(formatC(nrow(map),big.mark = ","),"/",
             formatC(n1,big.mark = ","),
             "rows remain after filtering with",
             paste0("top_n=",top_n,"."))
  } 
  #### Fill in label with any available info #####
  map[,label:=data.table::fcoalesce(object_label,subject_label)]
  return(map) 
}
