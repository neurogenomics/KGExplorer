#' @describeIn get_ get_
#' Monarch files
#' 
#' Find files 
#' \href{https://monarchinitiative.org/}{Monarch Initiative}
#' \href{https://data.monarchinitiative.org}{server}.
#' @param omit Files to omit from results.
#' 
#' @export
#' @import rvest
#' @examples
#' files <- get_monarch_files() 
get_monarch_files <- function(maps=NULL,
                              queries=NULL,
                              domain="https://data.monarchinitiative.org",
                              subdir="monarch-kg/latest/tsv/all_associations/",
                              omit=c("...","..","md5sums","index.html")
                              ){
  name <- subject <- object <- NULL;

  html <- rvest::read_html(paste(domain,subdir,sep="/"))
  links <- rvest::html_elements(html,"a")
  files <- data.table::data.table(
    name=gsub("\\.tsv|\\.gz","",rvest::html_text(links)),
    file=rvest::html_text(links),
    url=rvest::html_attr(links,"href")
  )
  #### Remove extra rows ####
  files <- files[!name %in% omit,]
  #### Steps specific to associations files ####
  if(grepl("_associations",subdir)){
    #### Add from/to cols ####
    files[,c("subject","object"):=data.table::tstrsplit(
      gsub("_association","",gsub("\\..*","",name)),
      "_to_" )]
    #### Subset using maps ####
    if(!is.null(maps)){
      messager("Filtering with `maps`.")
      files <- lapply(maps, function(m){
        if(length(m)<2) {
          stopper("Each map elements must be a vector of at least 2 items.")
        }
        if(!is.null(m)){
          files[(subject==m[1] & object==m[2]) | 
                (subject==m[2] & object==m[1])]
        } 
      }) |> data.table::rbindlist() |> unique()
    } 
  }  
  #### Subset using queries ####
  if(!is.null(queries)){
    messager("Filtering with `queries`.")
    bool <- lapply(queries, function(q){
      if(!is.null(q)){
        regex <- paste(paste0("*.",q,".*"),collapse = "&") 
        files[, Reduce(`|`,lapply(.SD, function(x){grepl(regex,x)}))] 
      } 
    }) |> data.table::as.data.table()
    files <- files[Matrix::rowSums(bool)>0,]
  }
  #### Report ####
  messager("Files found:",nrow(files))
  if(nrow(files)==0) stop("No files found.")
  #### Return ####
  return(files)
}
