% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0docs.R, R/link_monarch.R
\name{link_}
\alias{link_}
\alias{link_monarch}
\title{Link functions}
\usage{
link_monarch(
  maps = list(c("variant", "disease"), c("variant", "phenotypic_feature"), c("variant",
    "gene")),
  queries = NULL,
  node_filters = list(phenotype = NULL, subject_db = NULL, gene = NULL),
  as_graph = TRUE,
  domain = "https://data.monarchinitiative.org",
  subdir = "monarch-kg/latest/tsv/all_associations/",
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

\item{node_filters}{A named list of filters to apply to the node data. 
Names should be name of the metadata column, and values should be a vector of 
valid options. For example, \code{list("type" = c("gene","variant"))} will
return any rows where the "type" column contains either "gene" or "variant".}

\item{as_graph}{Return the object as a \link[tidygraph]{tbl_graph}.}

\item{domain}{Web domain to search for Monarch files.}

\item{subdir}{Subdirectory path to search for Monarch files within
\code{domain}.}

\item{...}{Not used at this time.}
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
dat <- link_monarch(maps = list(c("causal_gene","disease"),
                                c("correlated_gene","disease")))
}
\concept{link_}
