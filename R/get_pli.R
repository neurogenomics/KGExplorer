#' @describeIn get_ get_
#' Get pLI
#'
#' Get gene-level \href{https://gnomad.broadinstitute.org/faq#what-is-pli}{
#' pLI} scores for all canonical and non-canonical protein-coding gene
#' transcripts.
#' NOTE: The MANE Select set consists of one transcript at each protein-coding
#' locus across the genome that is representative of biology at that locus.
#' NOTE: Mapping genes with \link[orthogene]{map_genes} only reduces the number
#' of mapped genes compared to the provided "gene" column.
#' @inheritParams get_alphamissense
#' @export
#' @importFrom orthogene map_genes
#' @examples
#' \dontrun{
#' pli <- get_pli()
#' }
get_pli <- function(agg_fun=mean,
                    save_dir=cache_dir(),
                    force_new=FALSE){
  mane_select <- gene <- NULL;
  file <- "gnomad.v4.0.constraint_metrics.tsv"
  save_path <- file.path(save_dir,
                         gsub("\\.tsv",
                              paste0(".rds"),
                              file)
                         )
  if(file.exists(save_path) &&
     isFALSE(force_new)){
    messager("Loading cached pLI data:",shQuote(save_path))
    return(readRDS(save_path))
  }
  base_url <-paste0(
    "https://storage.googleapis.com/gcp-public-data--gnomad/",
    "release/v4.0/constraint/"
  )
  # readme <- suppressWarnings(readLines(paste0(base_url,"README.txt")))
  pli <- data.table::fread(paste0(base_url,file))
  mane <- pli[mane_select==TRUE, lapply(.SD, agg_fun),
              .SDcols = is.numeric, by="gene"][, mane_select:=TRUE]
  pli_agg <- data.table::rbindlist(
    list(
      mane,
      pli[!gene %in% mane$gene, lapply(.SD, agg_fun),
          .SDcols = is.numeric, by="gene"][, mane_select:=FALSE]
    )
  )
  #### Save ####
  if(!is.null(save_dir)){
    messager("Saving pLI data ==>",shQuote(save_path))
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(pli_agg,save_path)
  }
  #### Return ####
  return(pli_agg)
}
