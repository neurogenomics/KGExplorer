plot_upheno_rainplot <- function(plot_dat){

  requireNamespace("ggplot2")
  requireNamespace("ggdist")
  requireNamespace("tidyquant")
  subject_taxon_label2 <- n_phenotypes <- n_genes_intersect <- n_genes_db1 <-
    object_db <- gene_taxon_label2 <- db2 <- NULL;

  messager("Creating UPHENO rainplot")
  ### Plot proportion of intersecting orthologs per ontology ####
  ggplot2::ggplot(plot_dat,
         ggplot2::aes(x=paste0(gene_taxon_label2,
                      "\n(n = ",n_phenotypes," phenotypes)"),
             y=(n_genes_intersect/n_genes_db1),
             fill=factor(db2))) +
    ggplot2::facet_grid(rows=ggplot2::vars(db2),
                       scales = "free_y",
                       space = "free_y") +
    # add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      # adjust bandwidth
      adjust = 0.5,
      # move to the right
      justification = -0.1,
      # remove the slub interval
      .width = 0,
      point_colour = NA
    ) +
    ggplot2::geom_boxplot(
      show.legend = FALSE,
      width = 0.12,
      # removing outliers
      outlier.color = NA,
      alpha = 0.5
    ) +
    # ggdist::stat_dots(
    #   aes(color=factor(object_db)),
    #   show.legend = FALSE,
    #   # ploting on left side
    #   side = "left",
    #   # adjusting position
    #   justification = 1.1,
    #   # adjust grouping (binning) of observations
    #   # binwidth = 0.25
    # ) +
    # Themes and Labels
    tidyquant::scale_fill_tq() +
    tidyquant::scale_color_tq() +
    tidyquant::theme_tq() +
    ggplot2::coord_flip() +
    ggplot2::labs(x="Species",
                  fill="Ontology")
}
