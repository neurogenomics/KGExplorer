#' @describeIn map_ map_
#' Map phenotypes across uPheno
#'
#' Map phenotypes across species within the Unified Phenotype Ontology (uPheno).
#' First, gathers phenotype-phenotype mappings across ontologies.
#' Next, gathers all phenotype-gene associations for each ontology,
#' converts all genes to human HGNC orthologs, and computes the number of
#' overlapping orthologs between all pairs mapped phenotypes.
#' Finally, plots the results as the proportion of intersecting genes between
#' all pairs of phenotypes.
#' @source https://data.monarchinitiative.org/upheno2/current/qc/index
#' @source https://data.monarchinitiative.org/upheno2/current/upheno-release/all/index.html
#'
#' @param save_dir Directory to save cached data.
#' @param pheno_map_method Method to use for mapping phenotypes across ontologies.
#' \itemize{
#' \item{"upheno"}{Use uPheno's phenotype-to-phenotype mappings.
#'  Contains fewer ontologies but with greater coverage of phenotypes.}
#' \item{"monarch"}{Use Monarch's phenotype-to-phenotype mappings.
#' Contains more ontologies but with less coverage of phenotypes.
#' }
#' }
#' @param gene_map_method Method to use for mapping genes across species.
#' \itemize{
#' \item{"monarch"}{Use Monarch's gene-to-gene mappings.}
#' }
#' @param save_dir Directory to save cached data.
#' @param show_plot Show the plot.
#' @param fill_scores Fill missing scores in the "equivalence_score" and
#' "subclass_score" columns with this value. These columns represent the
#' quality of mapping between two phenotypes on a scale from 0-1.
#' @param terms A subset of HPO IDs to include in the final dataset and plots
#' (e.g. c("HP:0001508","HP:0001507")).
#' @returns A list containing the data and plot.
#'
#' @export
#' @import data.table 
#' @examples
#' library(ggplot2) # required step due to bug in ggdist
#' res <- map_upheno()
map_upheno <- function(pheno_map_method=c("upheno"),
                       gene_map_method=c("monarch"),
                       filters=list(db1="HP",
                                    gene_taxon_label1="Homo sapiens"),
                       terms=NULL,
                       fill_scores=NULL,
                       show_plot=TRUE,
                       force_new=FALSE,
                       save_dir=cache_dir()){
  #### Get data ####
  pheno_map_genes_match <- map_upheno_data(pheno_map_method=pheno_map_method,
                                           gene_map_method=gene_map_method,
                                           fill_scores=fill_scores,
                                           force_new = force_new,
                                           terms=terms)
  #### Plots ####
  if (show_plot){
    plots <- plot_upheno(pheno_map_genes_match,
                         filters=filters)
  } else{
    plots <- NULL
  }
  
  #### Return ####
  return(
    list(data=pheno_map_genes_match,
         plots=plots)
  )
}
