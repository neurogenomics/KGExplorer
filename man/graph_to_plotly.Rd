% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_to_plotly.R
\name{graph_to_plotly}
\alias{graph_to_plotly}
\title{igraph to plotly data}
\usage{
graph_to_plotly(
  g,
  ont = get_ontology("hpo"),
  layout_func = igraph::layout.fruchterman.reingold,
  dim = 3,
  id_col = "name",
  label_var = "hpo_name",
  seed = 2023
)
}
\arguments{
\item{g}{ggnetwork object 
(or an igraph/tbl_graph to be converted to ggnetwork format).}

\item{ont}{An ontology of class \link[simona]{ontology_DAG}.}

\item{layout_func}{Layout function for the graph.}

\item{dim}{Number of dimensions to create layout in.}

\item{id_col}{Column containing unique ID per term.}

\item{label_var}{Column containing the label for each node in a graph
(e.g. "hpo_name").}

\item{seed}{Set the seed for reproducible clustering.}
}
\value{
Named list of data.frames.
}
\description{
Convert an igraph to data for a 3D plotly plot.
}
\examples{
ont <- get_ontology("hp", terms=10)
g <- ontology_to(ont, to="tbl_graph")
p <- graph_to_plotly(g)
}
