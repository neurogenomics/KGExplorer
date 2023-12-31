plot_visnetwork <- function(g,
                            save_path,
                            layout,
                            solver,
                            physics,
                            forceAtlas2Based,
                            scaling,
                            smooth,
                            add_visExport,
                            degree,
                            height,
                            width,
                            randomSeed,
                            main,
                            submain){
  requireNamespace("visNetwork")
  . <- NULL;
  messager("Creating plot.")
  visnet <- visNetwork::toVisNetworkData(g) %>%
    {
      do.call(visNetwork::visNetwork,
              c(., list(main = main,
                        height = height,
                        width = width,
                        submain = submain,
                        background = "transparent")
                )
              )
    } |>
    # visNetwork::visIgraph(g,
    #                               randomSeed = randomSeed) |>
    visNetwork::visIgraphLayout(layout = layout,
                                type = "full",
                                randomSeed = randomSeed,
                                physics = physics) |>
    visNetwork::visPhysics(solver=solver,
                           forceAtlas2Based=forceAtlas2Based,
                           enabled = physics) |>
    visNetwork::visNodes(font = list(color="#F0FFFF",
                                     strokeWidth=2,
                                     strokeColor="rgba(0,0,0,1)"
                                     ),
    shadow = list(enabled=TRUE,
                  size = 10),
    opacity = 0.75,
    borderWidth=3,
    borderWidthSelected=6,
    color = list(hover = list(background="rgba(0,0,0,.5)"),
                 highlight = list(background="#00FFFFCF",
                                  border="#00FFFFCF")
                 ),
    scaling = scaling
    ) |>
    visNetwork::visEdges(shadow = list(enabled=FALSE),
                         smooth = smooth,
                         color = list(opacity = 0.5)) |>
    # visNetwork::visLegend() |>
    # visNetwork::visClusteringByConnection(nodes = unique(top_targets[[group_var]])) |>
    # visNetwork::visGroups(groupname = unique(igraph::vertex_attr(g,"group"))[[2]],
    #                       color="green")
    # visNetwork::visClusteringByGroup(groups = igraph::vertex_attr(g,"group"))
    visNetwork::visInteraction(hover = TRUE) |>
    visNetwork::visOptions(
                           selectedBy = "node",
                           highlightNearest = list(enabled=TRUE,
                                                   degree=degree)) |>
    visNetwork::visEvents(type = "on",
                          doubleClick = "function(){ this.fit()}")

  if(isTRUE(add_visExport)){
    visnet <-  visnet |> visNetwork::visExport(type = "pdf")
  }
  #### Save network ####
  if(!is.null(save_path)) {
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    messager("Saving plot ==>",save_path)
    visNetwork::visSave(visnet,
                        file = save_path,
                        selfcontained = TRUE,
                        background = "transparent")
    return(visnet)
  }
  #### Plot ####

  # {
  #   grDevices::pdf(file = "~/Downloads/network2.pdf")
  #   methods::show(visnet)
  #   grDevices::dev.off()
  # }
}
