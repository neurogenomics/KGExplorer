#' @describeIn plot_ plot_
plot_upheno_heatmap <- function(plot_dat,
                                lvl=10,
                                 ont=get_ontology("upheno",
                                                  lvl = lvl),
                                 hpo_ids=NULL,
                                 value.var=c("phenotype_genotype_score",
                                             "prop_intersect",
                                             "equivalence_score",
                                             "subclass_score"),
                                 name=value.var[1],
                                 min_rowsums=NULL,
                                 cluster_from_ontology=FALSE,
                                 save_dir=tempdir(),
                                 height = 15,
                                 width = 10){
  requireNamespace("ComplexHeatmap")
  hpo_id <- object_label1 <- id1 <- NULL;
  set.seed(2023)
  value.var <- match.arg(value.var)
  value.var <- value.var[1]
  
  messager("Creating UPHENO heatmap.") 

  ### Subset phenotypes
  if(!is.null(hpo_ids)) plot_dat <- plot_dat[id1 %in% unique(hpo_ids)]
  plot_dat[,hpo_id:=id1][,object_label1:=gsub(" (HPO)","",
                                              object_label1,fixed = TRUE)]
  if(isFALSE(cluster_from_ontology)){
    ont <- add_ancestors(ont, lvl = lvl)
    if(!"ancestor_name" %in% names(plot_dat)){
      plot_dat <- merge(
        plot_dat,
        ont@elementMetadata[,c("short_id","ancestor_name")],
        by.x="id1",
        by.y="short_id"
      )
    } 
  }
  data.table::setkeyv(plot_dat,"object_label1")

  ### Plot
  X <- data.table::dcast.data.table(plot_dat,
                                    formula = object_label1 ~ gene_taxon_label2,
                                    fill = 0,
                                    value.var = value.var,
                                    fun.aggregate = mean,
                                    na.rm=TRUE)|>
    dt_to_matrix()
  #### Filter by min_rowsums ####
  if(!is.null(min_rowsums)){
    X <- X[Matrix::rowSums(X, na.rm = TRUE)>=min_rowsums,]
  }
  if(nrow(X)==0) stop("No rows remain after filtering by `min_rowsums`.")

  #### Get clusters from ontology ####
  if(isTRUE(cluster_from_ontology)){
    nms <- map_ontology_terms(ont = ont,
                              terms = plot_dat$id1,
                              to = "name",
                              keep_order = FALSE)
    nms <- intersect(nms, rownames(X))
    ids <- map_ontology_terms(ont = ont,
                              terms =nms,
                              to = "id",
                              keep_order = FALSE)
    ## best to do this on the entire HPO, then subset 
    gd <- ontology_to(ont=ont,
                      to="igraph_dist",
                      terms = unname(ids))
    cluster_rows <- stats::hclust(d = as.dist(gd[unname(ids),unname(ids)]))
    # cluster_rows[cluster_rows$labels[1:10]]
    # leaves <- dendextend::get_leaves_attr(cluster_rows,"label")
    # c2 <- dendextend::prune(cluster_rows,
    #                         leaves[!leaves %in% names(ids)],
    #                         keep_branches = FALSE)
    ## subset to only those in the heatmap
    X <- X[nms,]
  } else {
    cluster_rows <- TRUE
  }
  #### Add annotation ####
  cols <- c("object_label1","n_genes_db1",
            "ancestor_name")
  cols <- cols[cols %in% names(plot_dat)]
  annot_dat <- plot_dat[rownames(X),
                        cols, with=FALSE] |> unique()
  # col_fun <- colorRamp2::colorRamp2(
  #   seq(min(annot_dat$n_genes_db1),
  #       max(annot_dat$n_genes_db1),
  #       length.out=4),
  #   pals::gnuplot(4))
  data.table::setnames(annot_dat,"n_genes_db1","N genes (HPO)")
  la <- ComplexHeatmap::rowAnnotation(
    df=data.frame(annot_dat[,-c("object_label1")],
                  row.names = annot_dat$object_label1, 
                  check.names = FALSE),
    show_legend = c(TRUE, FALSE)
    # annotation_legend_param = list(
    #   `N genes (HPO)`= list(title = "HPO genes"))
  )
  row_split <- if(isFALSE(cluster_from_ontology)) annot_dat$ancestor_name
  #### make heatmap ####
  ch <- ComplexHeatmap::Heatmap(matrix = as.matrix(X),
                                name = name,
                                cluster_rows = cluster_rows,
                                row_title_rot = 0,
                                row_names_gp = grid::gpar(fontsize = 7),
                                row_title_gp = grid::gpar(fontsize = 9),
                                row_split = row_split,
                                col = pals::viridis(n = 20),
                                cluster_columns = FALSE,
                                left_annotation = la)
  # methods::show(ch)
  #### Save ####
  if(!is.null(save_dir)){
    grDevices::pdf(file = file.path(save_dir,
                                    paste0("plot_upheno_heatmap-",
                                           value.var,".pdf")),
                   height = height,
                   width = width)
    methods::show(ch)
    grDevices::dev.off()
  }
  return(ch)
}
