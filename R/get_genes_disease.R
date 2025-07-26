#' @describeIn get_ get_
#' Load disease genes
#'
#' Load gene lists associated with each disease phenotype from:
#' \itemize{
#' \item{OMIM}
#' \item{Orphanet}
#' \item{DECIPHER}
#' }
#' @param run_map_mondo Run \link{map_mondo} to map MONDO IDs to disease IDs.
#' @param method Method to use for retrieving data. "tsv" for TSV files from the Monarch FTP server,
#' "kg" for the Monarch Knowledge Graph.
#' @inheritDotParams link_monarch
#' @returns data.table
#'
#' @export
#' @import data.table
#' @examples
#' genes_diseases <- get_genes_disease(method="tsv")
get_genes_disease <- function(maps = list(c("causal_gene","disease"),
                                          c("correlated_gene","disease")),
                              run_map_mondo = FALSE,
                              to = c("OMIM","Orphanet","DECIPHER"),
                              method=c("kg","tsv"),
                              ...){
  
  method <- match.arg(method)
  ## From Monarch Knowledge Graph via monarchr
  if(method=="kg"){
    # Get all diseases
    diseases <- monarchr::monarch_engine() |>
      monarchr::fetch_nodes(query_ids = c("MONDO:0000001"),
                            limit=NULL) |>
      monarchr::descendants(categories = "biolink:Disease")
    
    # Get all genes associated with diseases
    genes_diseases <- diseases |>
      monarchr::expand(categories="biolink:Gene")
    
    # Return graph
    return(genes_diseases)
    
  } else{ 
    # From TSV via FTP server
    disease_id <- to <- mondo_id <- NULL;
    
    #### Import data ####
    genes <- link_monarch(maps = maps,
                          as_graph = FALSE,
                          ...
    )
    # length(unique(genes$disease)) # 6244
    # genes2 <- get_monarch(queries = "gene_disease")[[1]]
    # length(unique(genes2$object))
    if(isTRUE(run_map_mondo)){
      genes <- map_mondo(dat = genes[,-c("label.to")],
                         input_col = "to",
                         output_col = "disease_id",
                         to = to)
      genes[,disease_id:=data.table::fcoalesce(disease_id,to)] 
      data.table::setnames(genes,"to","mondo_id")
      genes <- genes[grepl("^mondo",mondo_id, ignore.case = TRUE)]
      add_db(genes,
             input_col = "disease_id", 
             output_col = "disease_db")
      messager("disease(s):",data.table::uniqueN(genes$disease_id))
    }
    messager("genes(s):",data.table::uniqueN(genes$gene))  
    return(genes)
  } 
}
