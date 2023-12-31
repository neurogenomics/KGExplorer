% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/_docs.R, R/filter_chromosomes.R, R/filter_dt.R,
%   R/filter_kg.R, R/filter_ontology.R
\name{filter_}
\alias{filter_}
\alias{filter_chromosomes}
\alias{filter_dt}
\alias{filter_kg}
\alias{filter_ontology}
\title{Filter functions}
\usage{
filter_chromosomes(grlist, keep_chr = paste0("chr", c(seq_len(22), "X", "Y")))

filter_dt(dat, filters)

filter_kg(
  g,
  from_categories = paste0("biolink:", c("Disease", "PhenotypicFeature",
    "GrossAnatomicalStructure", "AnatomicalEntity", "Cell")),
  to_categories = from_categories,
  edge_categories = NULL,
  dbs = NULL,
  rm_isolated = TRUE,
  as_dt = FALSE
)

filter_ontology(
  ont,
  terms = NULL,
  remove_terms = NULL,
  use_simona = FALSE,
  ...
)
}
\arguments{
\item{grlist}{Named list of \link[GenomicRanges]{GRanges} objects.}

\item{keep_chr}{Which chromosomes to keep.}

\item{filters}{A named list, where each element in the list is the name of 
a column in the data, and the vector within each element represents the 
values to include in the final data.}

\item{g}{\link[tidygraph]{tbl_graph} object.}

\item{ont}{An ontology of class \link[simona]{ontology_DAG}.}

\item{terms}{Term IDs to include. Can alternatively be an integer, 
which will be used to randomly sample N terms from the data.}

\item{remove_terms}{Character vector of term IDs to exclude.}

\item{use_simona}{Use \link[simona]{dag_filter} to filter terms.}

\item{...}{Additional arguments passed to plot-specific functions.}
}
\value{
Converted data.

Named list of \link[GenomicRanges]{GRanges} objects.
}
\description{
Functions to filter objects
}
\section{Functions}{
\itemize{
\item \code{filter_chromosomes()}: filter_
Remove non-standard chromosomes

Remove non-standard chromosomes from a list of 
\link[GenomicRanges]{GRanges} objects.

\item \code{filter_dt()}: filter_

\item \code{filter_kg()}: filter_

Filter the monarch knowledge graph to only include edges between specific 
types of nodes (e.g. Disease <--> Cell).

\item \code{filter_ontology()}: filter_
Filter ontology

Filter ontology by terms.

}}
\examples{
g <- get_monarch_kg()
g2 <- filter_kg(g)
ont <- get_ontology("hp")
ont2 <- filter_ontology(ont,terms=c("HP:0000001","HP:0000002"))
ont3 <- filter_ontology(ont,terms=100)
}
\concept{filter_}
\keyword{internal}
