#' igraph to plotly data
#'
#' Convert an igraph to data for a 3D plotly plot.
#' @param dim Number of dimensions to create layout in.
#' @param id_col Column containing unique ID per term.
#' @inheritParams plot_graph_3d
#' @returns Named list of data.frames.
#'
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' g <- ontology_to(ont, to="tbl_graph")
#' p <- graph_to_plotly(g)
graph_to_plotly <- function(g,
                            ont=get_ontology("hpo"),
                            layout_func = igraph::layout.fruchterman.reingold,
                            dim = 3,
                            id_col="name",
                            label_var="hpo_name",
                            seed = 2023){

  requireNamespace("igraph")
  .SD <- NULL;

  set.seed(seed)
  messager("Converting igraph to plotly data.")
  # xyz <- ggplot2::fortify(g) ## Only works with 2 dimensions
  layout_coords <- cbind(
    name=names(igraph::V(g)),
    layout_func(g,dim=dim) |>
      data.table::as.data.table()|>
      `colnames<-`(c("x","y","z")[seq(dim)])
  )
  # d <- data.table::as.data.table(igraph::as_edgelist(g))
  d <- igraph::as_data_frame(g, what = "both")
  # d <- igraph::as_long_data_frame(g)
  # gnet <- data.table::data.table(
  #   graph_to_ggnetwork(g)
  # )[,-c("x","y","xend","yend")]
  # if("node_type" %in% names(gnet)){
  #   gnet <- unique(gnet[node_type %in% c("hpo_name","ancestor_name")])
  # }

  #### Vertex data ####
  #### Merge layout coordinates with vertex metadata ####
  vdf <- merge(
    layout_coords,
    d$vertices,
    by = id_col,
    all = TRUE
  )
  ## Make NAs into characters so that they still get assigned a point color
  cols <- names(vdf)[sapply(vdf, class) == 'character']
  vdf[, (cols) := lapply(.SD, replace,NA,"NA"),.SDcols=cols]
  #### Edge data ####
  edf <- data.table::merge.data.table(
    data.table::data.table(d$edges),
    layout_coords,
    by.x="from",
    by.y=id_col
  ) |> data.table::merge.data.table(
    data.table::copy(layout_coords) |>
      data.table::setnames(c("x","y","z")[seq(dim)],
                           c("xend","yend","zend")[seq(dim)]
                           ),
    by.x="to",
    by.y=id_col
  )

  if(!label_var %in% names(vdf)){
    vdf[[label_var]] <- map_ontology_terms(ont,
                                           terms = vdf[[id_col]])
  }
  return(list(vertices=vdf,
              edges=edf))
}
