plot_upheno_scatterplot <- function(plot_dat){

  requireNamespace("ggplot2")
  n_genes_intersect <- n_genes_db1 <- equivalence_score <- NULL;
  
  messager("Creating UPHENO scatterplot")
  #### Check if there's a relationship between phenotype mapping scores
  # and number of shared genes ####
  ggplot2::ggplot(plot_dat,
                  ggplot2::aes(x=(n_genes_intersect/n_genes_db1),
             y=equivalence_score)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(rows = "gene_taxon_label2") +
    ggplot2::geom_smooth()
}
