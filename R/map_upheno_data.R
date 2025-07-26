#' Map uPheno data
#'
#' Get uPheno cross-species mapping data by:
#' \itemize{
#' \item{Downloading cross-species phenotype-phenotype mappings.}
#' \item{Downloading within-spceies phenotype-gene mappings,
#' and converting these genes to human orthologs.}
#' \item{Merging the phenotype-phenotype and phenotype-gene mappings.}
#' \item{Filtering out any mappings that do not have a human ortholog within
#' each respective phenotype.}
#' \item{Calculating the proportion of orthologous genes overlapping across
#' the species-specific phenotype-gene mappings.}
#' \item{Iterating the above steps using multiple methods
#' (\code{pheno_map_method}) and concatenating the results together.}
#' }
#' @param keep_nogenes Logical indicating whether to keep mappings that do not
#' have any orthologous genes.
#' @param agg Aggregate the data to to phenotype-level (TRUE). 
#' Otherwise, keep data at gene level (FALSE).
#' @inheritParams map_upheno
#' @returns A data.table containing the mapped data.
#'
#' @export
#' @examples
#' \dontrun{
#' pheno_map_genes_match <- map_upheno_data()
#' }
map_upheno_data <- function(pheno_map_method=c("upheno"),#"monarch"
                            gene_map_method=c("monarch"),
                            keep_nogenes=FALSE,
                            fill_scores=NULL,
                            terms=NULL,
                            save_dir=cache_dir(),
                            agg=TRUE,
                            force_new=FALSE){
  #### Check for cached data ####
  save_path <- file.path(
    save_dir,
    paste0("pheno_map_genes_match",if(isFALSE(agg)) ".noAgg",".rds"))
  if(file.exists(save_path) &&
     isFALSE(force_new)){
    ## Read from cache
    messager("Importing cached data:",save_path)
    pheno_map_genes_match <- readRDS(save_path)
  } else {
    ## Create
    pheno_map_genes_match <- lapply(
      stats::setNames(pheno_map_method,
                      pheno_map_method),
      function(m){
        map_upheno_data_i(pheno_map_method=m,
                          gene_map_method=gene_map_method,
                          keep_nogenes=keep_nogenes,
                          fill_scores=fill_scores,
                          terms=terms,
                          agg=agg)
      }) |> data.table::rbindlist(fill=TRUE, 
                                  idcol = "pheno_map_method")
    ## Save
    messager("Caching processed file -->",save_path)
    attr(pheno_map_genes_match,"version") <- format(Sys.Date(), "%Y-%m-%d")
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(pheno_map_genes_match,save_path)
  }
  ## Return
  return(pheno_map_genes_match)
}
