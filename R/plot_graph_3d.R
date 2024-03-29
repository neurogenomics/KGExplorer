#' @describeIn plot_ plot_
#' 3D network
#'
#' Plot a subset of the HPO as a 3D network.
#' @returns A 3D interactive \link[plotly]{plotly} object.
#'
#' @source
#' \href{https://www.r-bloggers.com/2013/09/network-visualization-part-4-3d-networks/}{
#' R bloggers}
#' @source
#' \href{https://community.plotly.com/t/is-it-possible-to-connect-scatters-in-3d-scatter-plot/}{
#' Plotly: Connect points in 3D plot}
#' @source \href{https://plotly.com/r/line-charts/}{
#' Plotly: Connect points in 2D plot}
#' @source \href{https://plotly.com/r/3d-line-plots/}{
#' Plotly: 3D lines plots}
#' @source \href{https://plotly.com/python/v3/3d-network-graph/}{
#' Plotly: 3D network plot}
#'
#' @export
#' @importFrom stringr str_wrap
#' @importFrom methods show
#' @examples
#' ont <- get_ontology("hp", terms=10, add_ancestors=TRUE)
#' g <- ontology_to(ont, to="tbl_graph")
#' plt <- plot_graph_3d(g=g, show_plot=FALSE)
plot_graph_3d <- function(g,
                          layout_func = igraph::layout.fruchterman.reingold,
                          id_var = "name",
                          node_color_var = "ancestor_name",
                          edge_color_var = "zend",
                          text_color_var = node_color_var,
                          node_symbol_var = "ancestor_name",
                          node_palette = pals::kovesi.cyclic_mrybm_35_75_c68_s25,
                          edge_palette = pals::kovesi.cyclic_mrybm_35_75_c68_s25,
                          node_opacity = .75,
                          edge_opacity = .5,
                          kde_palette = pals::gnuplot,
                          add_kde = TRUE,
                          extend_kde = 1.5,
                          bg_color = kde_palette(6)[1],
                          add_labels = FALSE,
                          keep_grid = FALSE,
                          aspectmode = 'cube',
                          hover_width=100,
                          label_width=100,
                          seed = 2023,
                          showlegend = TRUE,
                          show_plot = TRUE,
                          save_path = tempfile(fileext = "plot_graph_3d.html"),
                          verbose = TRUE){
  # anc <- "Abnormality of the nervous system"
  # dat <- make_phenos_dataframe(ancestor = anc)[ancestor_name==anc,]
  # g <- make_igraph_object(dat = dat)
  # g <- readRDS("~/Downloads/priotised_igraph.rds")
  requireNamespace("plotly")
  requireNamespace("pals")
  requireNamespace("htmlwidgets")
  label <- hpo_name <- NULL;

  #### Convert igraph to plotly data ####
  d <- graph_to_plotly(g = g,
                       layout_func = layout_func,
                       dim = 3,
                       seed = seed)
  vdf <- d$vertices
  vdf[,label:=gsub("\n","<br>",
                   stringr::str_wrap(hpo_name, width = label_width))]
  edf <- d$edges
  remove(d)
  #### Create paths and nodes ####
  fig <-
    plotly::plot_ly() |>
    plotly::add_paths(data = edf,
                      x = ~xend,
                      y = ~yend,
                      z = ~zend,
                      xend = ~xend,
                      yend = ~yend,
                      # zend = ~zend,
                      color = ~get(edge_color_var),
                      colors = rev(edge_palette(50)),
                      line = list(shape = "spline"),
                      type = "scatter3d",
                      name = "edge",
                      opacity = edge_opacity,
                      mode = "lines",
                      hoverinfo = "none",
                      showlegend = FALSE,
                      inherit = FALSE) |>
    plotly::add_markers(data = vdf,
                        x = ~x,
                        y = ~y,
                        z = ~z,
                        # symbol = ~stringr::str_wrap(
                        #   get(node_symbol_var),
                        #   width = label_width
                        # ),
                        # size = ~ontLvl,
                        color = ~ get(node_color_var),
                        colors = (
                          node_palette(length(
                            unique(vdf[[node_color_var]])
                          ))
                        ),
                        marker = list(
                          line = list(
                            color = bg_color
                          )
                        ),
                        hovertext = ~ stringr::str_wrap(
                          paste(
                            paste0('<b>',id_var,'</b>: ',id_var),
                            paste0('<b>hpo_name</b>: ',hpo_name),
                            paste0('<b>ontLvl</b>: ',ontLvl),
                            paste0('<b>ancestor_name</b>: ',ancestor_name),
                            sep = "<br>"),
                          width = hover_width
                        ),
                        opacity = node_opacity,
                        showlegend = showlegend,
                        type = "scatter3d",
                        mode = "markers")
  #### Add KDE ####
  if(isTRUE(add_kde)){
    kd <- kde_surface(xyz = vdf,
                      extend_kde = extend_kde)
    fig <- fig |>
      plotly::add_surface(data = kd,
                          x = ~x,
                          y = ~y,
                          z = ~z,
                          opacity = 1,
                          hoverinfo = "none",
                          colorscale = list(
                            seq(0,1,length.out=6),
                            kde_palette(n = 6)
                          ),
                          showlegend = FALSE,
                          inherit = FALSE)
  }
  #### Add text ####
  if(isTRUE(add_labels)){
    fig <- fig |>
      plotly::add_text(data = vdf,
                       x = ~x,
                       y = ~y,
                       z = ~z,
                       text = ~label,
                       color = ~ get(text_color_var),
                       colors = 
                         node_palette(length(
                           unique(vdf[[text_color_var]])
                           )
                         ),
                       # textfont = list(color="rbga(255,255,255,.8"),
                       hoverinfo = "none",
                       inherit = FALSE,
                       showlegend = FALSE)
  }
  #### Add layout ####
  scene <- lapply(stats::setNames(c("xaxis","yaxis","zaxis"),
                                  c("xaxis","yaxis","zaxis")),
                  function(x){list(showgrid = keep_grid,
                                   showline = keep_grid,
                                   showspikes = keep_grid,
                                   zeroline = keep_grid,
                                   title = list(
                                     text=if(isTRUE(keep_grid)){
                                       x
                                     } else {""}
                                   ),
                                   showticklabels = keep_grid)}
  )
  scene$aspectmode <- aspectmode
  fig <- fig |>
    plotly::layout(
      hoverlabel = list(align = "left"),
      plot_bgcolor = bg_color,
      paper_bgcolor = bg_color,
      scene = scene
      # showlegend = FALSE
    ) |>
    plotly::colorbar(title=edge_color_var)
  # file <- file.path("~/Downloads","hpo_plotly.png")
  # plotly::export(p = fig,
  #                file = file,
  #                selenium = RSelenium::rsDriver(browser = "chrome"))

  # reticulate::py_install(packages = "kaleido",)
  # plotly::save_image(p = fig,file = file, width = 10, height =10)
  if(isTRUE(show_plot)) methods::show(fig)
  if(!is.null(save_path)) {
    messager("Saving interactive plot -->",save_path)
    dir.create(dirname(save_path),showWarnings = FALSE, recursive = TRUE)
    htmlwidgets::saveWidget(widget = fig,
                            file = save_path,
                            selfcontained = TRUE)
  }
  return(fig)
}
