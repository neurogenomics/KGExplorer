link_monarch_graph <- function(files,
                               maps,
                               as_graph=TRUE,
                               ...){
  subject <- object <- subject_category <- object_category <- NULL;
  #### Download and prepare maps ####
  map_dt <- lapply(seq(length(maps)), function(i){
    m <- maps[[i]]
    messager("Constructing data:",m[1],"<-->",m[2])
    URL <- files[(subject==m[1] & object==m[2]) |
                 (subject==m[2] & object==m[1])
    ][1,]$url
    d <- data.table::fread(URL)
    d[,subject_category:=m[1]][,object_category:=m[2]]
    d[,file:=basename(URL)]
    add_db(dat = d, 
           input_col = "subject", 
           output_col = "subject_db") 
    add_db(dat = d, 
           input_col = "object", 
           output_col = "object_db") 
    if(isTRUE(as_graph)){
      g <- dt_to_graph(dat = d)
      return(g)
    } else {
      return(d)
    }
  })
  
  #### Merge graphs/tables ####
  if(isTRUE(as_graph)){
    g <- Reduce(tidygraph::graph_join, map_dt, ...) 
    return(g)
  } else {
    d <- data.table::rbindlist(map_dt, fill=TRUE)
    return(d)
  }
}