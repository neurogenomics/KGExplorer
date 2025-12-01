#' @describeIn get_ get_
#' Get GenCC
#'
#' Get phenotype-gene evidence score from the
#' \href{https://thegencc.org/}{Gene Curation Coalition}.
#' Note that the column "submitted_as_moi_id" indicates the mechanism of action
#'  (e.g. "Autosomal dominant inheritance"), not specific HPO phenotypes.
#' Set \code{agg_by=NULL} to return raw unaggregated data.
#' 
#' Data downloaded from \href{https://search.thegencc.org/download}{here}.\cr
#' \emph{NOTE:} Due to licensing restrictions, a GenCC download does not
#'  include OMIM data. OMIM data can be accessed and downloaded through
#'  \href{https://omim.org/downloads/}{OMIM}.\cr
#' \emph{NOTE:} GenCC does not currently have any systematic versioning.
#' There for the \code{attr(obj,"version")} attribute is set to the date it was
#' downloaded and cached by \link{get_gencc}.
#'
#' @param dict A named vector of evidence score mappings.
#' See \href{https://thegencc.org/faq.html#validity-termsdelphi-survey}{here}
#' for more information. 
#' @returns \link[data.table]{data.table} with columns:
#' \describe{
#' \item{"disease_id"}{Disease ID.}
#' \item{"gene_symbol"}{Gene symbol.}
#' \item{"evidence_score"}{Evidence score.}
#' }
#'
#' @export
#' @import data.table
#' @importFrom utils download.file
#' @importFrom stats sd
#' @examples
#' d <- get_gencc()
get_gencc <- function(agg_by=c("disease_id",
                               "gene_symbol"),
                      dict = c("Definitive"=6,# GENCC:100001
                               "Strong"=5, # GENCC:100002
                               "Moderate"=4, # GENCC:100003
                               "Supportive"=3, # GENCC:100009
                               "Limited"=2, # GENCC:100004
                               "Disputed Evidence"=1, # GENCC:100005
                               "Refuted Evidence"=0, # GENCC:100006
                               "No Known Disease Relationship"=0 # GENCC:100008
                      ),
                      save_dir=cache_dir(),
                      force_new=FALSE){
  disease_id <- disease_original_curie <- classification_title <-
    submitted_as_moi_id <- evidence_score <- hpo_id <- NULL;

  messager("Gathering data from GenCC.")
  URL <- "https://search.thegencc.org/download/action/submissions-export-csv"
  f <- file.path(save_dir,"genCC_submission.csv")
  if(!file.exists(f) || isTRUE(force_new)){
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    utils::download.file(URL, f)
  } else {
    messager("Importing cached file.")
  }
  d <- data.table::fread(f)
  d[,disease_id:=gsub("^Orphanet","ORPHA",disease_original_curie)]
  # d <- map_mondo(dat = d,  
  #                input_col = "disease_original_curie",
  #                output_col = "disease_id",
  #                to = c("OMIM","DECIPHER","Orphanet"))
  #### Encode evidence numerically ####
  d[,evidence_score:=dict[classification_title]]
  #### Report ####
  messager("Evidence scores for:",
           "\n -",length(unique(d$disease_id)),"diseases",
           "\n -",length(unique(d$gene_symbol)),"genes")
  #### Aggregate so that there's 1 entry/gene/disease ####
  if(!is.null(agg_by)){
    d <- d[,list(evidence_score_min=min(evidence_score, na.rm = TRUE),
                 evidence_score_max=max(evidence_score, na.rm = TRUE),
                 evidence_score_mean=mean(evidence_score, na.rm=TRUE),
                 evidence_score_sum=sum(evidence_score, na.rm=TRUE),
                 evidence_score_sd=stats::sd(evidence_score, na.rm=TRUE)),
               by=agg_by]
  }
  #### Add version ####
  attr(d,"version") <- format(file.info(f)$mtime, "%Y-%m-%d")
  get_version(obj=d)
  #### Return ####
  return(d)
}
