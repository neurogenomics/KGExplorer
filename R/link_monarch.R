#' @describeIn link_ link_
#' Link Monarch
#'
#' Construct a knowledge graph by iteratively linking together pairs of concepts
#'  across  multiple Monarch datasets.
#' @export
#' @examples
#' dat <- link_monarch(maps = list(c("causal_gene","disease"),
#'                                 c("correlated_gene","disease")))
link_monarch <- function(maps = list(
                                    c("variant","disease"),
                                    c("variant","phenotypic_feature"),
                                    c("variant","gene")
                         ),
                         queries=NULL,
                         node_filters=
                           list(
                             phenotype=NULL,
                             subject_db=NULL,#c("HP"),
                             gene=NULL
                           ),
                         as_graph=TRUE,
                         domain="https://data.monarchinitiative.org",
                         subdir="monarch-kg/latest/tsv/all_associations/",
                         ...
                         ){
  
  files <- get_monarch_files(maps = maps,
                             queries = queries,
                             domain = domain,
                             subdir = subdir) 
  g <- link_monarch_graph(files=files,
                          maps=maps,
                          as_graph=as_graph,
                          ...)
  if(isTRUE(as_graph)){
    g <- filter_graph(g,
                      node_filters=node_filters)
  } else {
    g <- filter_dt(dat=g,
                   filters=node_filters)
  }
  return(g)
}
