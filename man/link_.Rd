% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/_docs.R, R/link_monarch.R
\name{link_}
\alias{link_}
\alias{link_monarch}
\title{Link functions}
\usage{
link_monarch(
  maps = list(c("variant", "disease"), c("variant", "phenotype"), c("variant", "gene")),
  queries = NULL,
  filters = list(phenotype = NULL, db = NULL, gene = NULL),
  as_graph = TRUE,
  domain = "https://data.monarchinitiative.org",
  subdir = "latest/tsv/all_associations/",
  ...
)
}
\arguments{
\item{maps}{A list of paired to/from types to filter Monarch association 
files by. For example, \code{list(c("gene","disease"))} will return any 
 files that contains gene-disease associations.
Passes to \link{get_monarch_files}.}

\item{queries}{A list of free-form substring queries to filter files by 
(using any column in the metadata). 
For example, \code{list("gene_disease","variant_disease")} will return any 
 files that contain either of the substrings 
 "gene_disease" or "variant_disease".
Passes to \link{get_monarch_files}.}

\item{filters}{A named list, where each element in the list is the name of 
a column in the data, and the vector within each element represents the 
values to include in the final data.}

\item{as_graph}{Return the object as a \link[tidygraph]{tbl_graph}.}

\item{domain}{Web domain to search for Monarch files.}

\item{subdir}{Subdirectory path to search for Monarch files within
\code{domain}.}

\item{...}{Additional arguments passed to plot-specific functions.}
}
\value{
Merged data.
}
\description{
Functions to merge data resources.
}
\section{Functions}{
\itemize{
\item \code{link_monarch()}: link_
Link Monarch

Construct a knowledge graph by iteratively linking together pairs of concepts
 across  multiple Monarch datasets.

}}
\examples{
dat <- link_monarch()
}
\concept{link_}
