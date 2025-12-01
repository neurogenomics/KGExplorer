#' @describeIn plot_ plot_
#' Plot heatmap
#'
#' Plot a phenotype x phenotype correlation matrix based on genetic overlap. 
#' @param row_side_vars Variables to include in row-side
#' metadata annotations.
#' @param col_side_vars Variables to include in column-side
#' metadata annotations.
#' @param fontsize Axis labels font size.
#' @param seed Set the seed for reproducible clustering.
#' @inheritParams map_
#' @inheritParams ComplexHeatmap::Heatmap
#' @inheritParams grDevices::pdf
#' @returns Plot
#'
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=2)
#' hm <- plot_ontology_heatmap(ont)
plot_ontology_heatmap <- function(ont,
                                  annot = data.table::data.table(
                                    as.data.frame(ont@elementMetadata)
                                    ),
                                  X = ontology_to(ont, to = "similarity"),
                                  fontsize = ont@n_terms*4e-4,
                                  row_labels = ont@terms,
                                  column_labels = row_labels,
                                  name = NULL,
                                  row_side_vars = c("ancestor_name"),
                                  col_side_vars = c("IC",
                                                    "depth",
                                                    "n_children",
                                                    "n_offspring",
                                                    "n_connected_leaves"),
                                  col = pals::gnuplot(),
                                  show_plot = TRUE,
                                  save_path = tempfile(
                                    fileext = "plot_ontology_heatmap.pdf"),
                                  height = 12,
                                  width = height*1.1,
                                  # row_km = 3,
                                  # column_km = row_km,
                                  # row_km_repeats = 1000,
                                  # column_km_repeats = row_km_repeats,
                                  seed = 2023,
                                  types = c("heatmaply",
                                            "ComplexHeatmap")[2],
                                  ...
                                  ){
  if(!is.null(seed)) set.seed(seed)
  ## Check if we need to add ancestors
  if(any(c("ancestor","ancestor_name") %in% c(row_side_vars,col_side_vars))
     && !is.null(ont)){
    ont <- add_ancestors(ont)
  }
  #### Heatmaply version ####
  if("heatmaply" %in% types){
    requireNamespace("heatmaply")
    messager("Creating heatmap: heatmaply")
    X[is.na(X)] <- 0
    hm <- heatmaply::heatmaply(X,
                               row_side_colors = annot[,row_side_vars,
                                                       with=FALSE],
                               labRow = row_labels,
                               labCol = column_labels,
                               # k_row = row_km,
                               # k_col = column_km,
                               colors = col,
                               ...)
    #### Plotly version ####
    # plotly::plot_ly() |>
    #   plotly::add_heatmap(x=colnames(X_cor),
    #                       y=rownames(X_cor),
    #                       z=X_cor)
  #### ComplexHeatmap version ####
  } else {
    requireNamespace("ComplexHeatmap")
    requireNamespace("grid")
    messager("Creating heatmap: ComplexHeatmap")
    ra <- make_rannot(annot = annot,
                      row_side_vars = row_side_vars)
    ca <- make_cannot(annot = annot,
                      col_side_vars = col_side_vars)
    hm <- ComplexHeatmap::Heatmap(matrix = X,
                                  name = name,
                                  col = col,
                                  right_annotation = ra,
                                  top_annotation = ca,
                                  row_dend_side = "left",
                                  column_names_gp =
                                    grid::gpar(fontsize = fontsize),
                                  row_names_gp =
                                    grid::gpar(fontsize = fontsize),
                                  # row_km = row_km,
                                  row_labels = row_labels,
                                  column_labels = column_labels,
                                  # column_km = column_km,
                                  # row_km_repeats = row_km_repeats,
                                  # column_km_repeats = column_km_repeats,
                                  gap = grid::unit(.5, "mm"),
                                  row_gap = grid::unit(0, "mm"),
                                  column_gap = grid::unit(0, "mm"),
                                  cluster_rows = TRUE,
                                  cluster_columns = TRUE,
                                  cluster_row_slices = FALSE,
                                  cluster_column_slices = FALSE,
                                  row_dend_reorder = FALSE,
                                  column_dend_reorder = FALSE,
                                  ...)
    # ComplexHeatmap::row_order(hm)
    #### Save plot ####
    if(!is.null(save_path)){
      plot_save(plt = hm,
                save_path = save_path,
                height = height,
                width = width)
    }
  }
  #### Show plot ####
  if(isTRUE(show_plot)) {
    methods::show(hm)
    if(file.exists(save_path)) utils::browseURL(save_path)
  }
  #### Return ####
  return(hm)
}
