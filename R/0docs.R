#### main_ ####
#' @title Main functions
#' 
#' @description
#' Documentation for common arguments.
#' @param ont An ontology of class \link[simona]{ontology_DAG}. 
#' @param terms A vector of ontology term IDs.
#' @param g \link[tidygraph]{tbl_graph} object.
#' @param force_new Create a new file instead of using any cached files.
#' @param save_dir Directory to save a file to.
#' @param save_path File name to save to.
#' @param obj R object.
#' @family main_ 
#' @returns R object.
#' 
#' @name main_
NULL



#### utils_ ####
#' @title Utility functions
#' 
#' @description
#' Functions to support other functions.
#' @family utils_ 
#' @returns Merged data.
#' 
#' @name utils_
NULL

#### query_ ####
#' @title Query functions
#' 
#' @description
#' Functions to query specific subset of data via dedicated APIs.
#' @param ids IDs to query.
#' @param batch_size Number of IDs to query at once.
#' 
#' @family query_ 
#' @returns Queried data.
#' 
#' @name query_
NULL

#### plot_ ####
#' @title Plot functions
#' 
#' @description
#' Functions to plot data.
#' @param types Types of graph to produce. Can be one or more.
#' @param layout_func Layout function for the graph.
#' @param node_color_var Variable in the vertex metadata to color nodes by.
#' @param edge_color_var Variable in the edge metadata to color edges by.
#' @param text_color_var Variable in the node metadata to color text by.
#' @param node_symbol_var Variable in the vertex metadata to shape nodes by.
#' @param node_opacity Node opacity.
#' @param edge_opacity Edge opacity.
#' @param node_palette Color palette function for the nodes/points.
#' @param edge_palette Color palette function for the edges/lines.
#' @param kde_palette Color palette function for the KDE plot.
#' @param add_kde Add a kernel density estimation (KDE) plot
#' below the 3D scatter plot (i.e. the "mountains" beneath the points).
#' @param extend_kde Extend the area that the KDE plot covers.
#' @param bg_color Plot background color.
#' @param add_labels Add phenotype name labels to each point.
#' @param keep_grid Keep all grid lines and axis labels.
#' @param aspectmode The proportions of the 3D plot. See the
#' \href{https://plotly.com/python/reference/layout/scene/#layout-scene-aspectmode}{
#' plotly documentation site} for details.
#' @param hover_width Maximum width of the hover text.
#' @param label_width Maximum width of the label text.
#' @param seed Random seed to enable reproducibility.
#' @param showlegend Show node fill legend.
#' @param show_plot Print the plot after it's been generated.
#' @param save_path Path to save interactive plot to
#' as a self-contained HTML file.
#' @param interactive Make the plot interactive.
#' @param verbose Print messages.
#' @param label_var Column containing the label for each node in a graph
#'  (e.g. "hpo_name").
#' @param size_var Column to scale node size by.
#' @param colour_var Column to color nodes by.
#' @inheritParams main_
#' @inheritParams filter_
#' @import simona
#' @family plot_ 
#' @returns A named list containing the plot and the data.
#' 
#' @name plot_
NULL

#### get_ ####
#' @title Get functions
#' 
#' @param maps A list of paired to/from types to filter Monarch association 
#' files by. For example, \code{list(c("gene","disease"))} will return any 
#'  files that contains gene-disease associations.
#' Passes to \link{get_monarch_files}.
#' @param queries A list of free-form substring queries to filter files by 
#' (using any column in the metadata). 
#' For example, \code{list("gene_disease","variant_disease")} will return any 
#'  files that contain either of the substrings 
#'  "gene_disease" or "variant_disease".
#' Passes to \link{get_monarch_files}.
#' @param domain Web domain to search for Monarch files.
#' @param subdir Subdirectory path to search for Monarch files within
#'  \code{domain}.
#' @param rbind If \code{TRUE}, rbinds all \link[data.table]{data.table}s 
#' together. Otherwise, returns a named list of separated 
#' \link[data.table]{data.table}s.
#' @param agg_by Column names to aggregate results by.
#' @param run_map_genes Map genes to standardised HGNC symbols using 
#' \link[orthogene]{map_genes}.
#' @param from The designated from column in from-to mapping or relations.
#' @param include_self For dag_offspring() and dag_ancestors(), this controls 
#' whether to also include the query term itself. 
#' @inheritParams main_
#' @inheritParams add_
#' @inheritParams to_
#' @inheritParams map_ 
#' @inheritParams data.table::merge.data.table
#' @import rols
#' @description
#' Functions to get data resources.
#' @family get_ 
#' @returns Data.
#' 
#' @name get_
#' @import data.table
NULL

#### map_ ####
#' @title Map functions
#' 
#' @description
#' Functions to map IDs across ontologies/databases.
#' @param top_n Top number of mappings to return per \code{top_by} grouping.
#' Set to \code{NULL} to skip this step.
#' @param map_types Mapping types to include.
#' @param map_to Mapping outputs to include
#'  (from Mondo IDs to another database's IDs).
#' @param input_col Column name of input IDs.
#' @param output_col Column name of output IDs.
#' @param add_name Logical, if TRUE, add mondo name column.
#' @param add_definitions logical, if TRUE, add mondo definition column.  
#' @param gr A \link[GenomicRanges]{GRanges} object.
#' @param build Genome build to use when mapping genomic coordinates.
#' @inheritParams main_
#' @inheritParams to_
#' @inheritParams filter_
#' @inheritParams data.table::merge.data.table
#' @inheritParams VariantAnnotation::locateVariants
#' @inheritParams VariantAnnotation::PromoterVariants
#' @family map_ 
#' @returns Mapped data. 
#' @name map_
#' @import data.table
NULL


#### filter_ ####
#' @title Filter functions
#' 
#' @description
#' Functions to filter objects
#' @param remove_terms Character vector of term IDs to exclude.
#' @param use_simona Use \link[simona]{dag_filter} to filter terms.
#' @param keep_chr Which chromosomes to keep.
#' @param grlist Named list of \link[GenomicRanges]{GRanges} objects.
#' @param filters A named list, where each element in the list is the name of 
#' a column in the data, and the vector within each element represents the 
#' values to include in the final data.
#' @param node_filters A named list of filters to apply to the node data. 
#' Names should be name of the metadata column, and values should be a vector of 
#' valid options. For example, \code{list("type" = c("gene","variant"))} will
#' return any rows where the "type" column contains either "gene" or "variant".
#' @param edge_filters A named list of filters to apply to the edge data. 
#' @param keep_descendants Terms whose descendants should be kept 
#' (including themselves).
#'  Set to \code{NULL} (default) to skip this filtering step.
#' @param remove_descendants Terms whose descendants should be removed 
#' (including themselves).
#'  Set to \code{NULL} (default) to skip this filtering step.
#' @inheritParams main_
#' @inheritParams plot_
#' @inheritParams get_
#' @import simona
#' @family filter_ 
#' @returns Converted data. 
#' @name filter_
NULL

#### to_ ####
#' @title To functions
#' 
#' @description
#' Functions to convert one object type to another. 
#' @param to A character string specifying the format to convert to.
#' @param as A character string specifying the format to convert to.
#' @param as_dt Return the object as a \link[data.table]{data.table}.
#' @param as_graph Return the object as a \link[tidygraph]{tbl_graph}.
#' @param as_sparse Return the object as a \link[Matrix]{sparseMatrix}. 
#' @param as_granges Return the object as a \link[GenomicRanges]{GRanges}.
#' @inheritParams main_
#' @inheritParams plot_
#' @inheritParams filter_
#' @import tidygraph
#' @import simona
#' @family to_ 
#' @returns Converted data. 
#' @name to_
NULL

#### add_ ####
#' @title Add functions
#' 
#' @description
#' Functions to add extra metadata to an ontology or data.table object. 
#' @param ont An ontology of class \link[simona]{ontology_DAG}. 
#' @param terms A vector of ontology term IDs.
#' @param g \link[tidygraph]{tbl_graph} object.
#' @param force_new Add the data again even if the associated column already 
#' exists.
#' @param lvl Depth of the ancestor terms to add. 
#' Will get the closest ancestor to this level if none have this exact level.
#' @param add_ancestors Add ancestors for each term.
#' @param add_n_edges Add the number of edges (connections) for each term.
#' @param add_ontology_levels Add the ontology level for each term.
#' @import simona
#' @family add_ 
#' @returns Added data. 
#' @name add_
NULL

#### cache_ ####
#' @title Cache functions
#' 
#' @description
#' Functions to cache objects in order to speed up processes the second time. 
#' @inheritParams main_
#' @inheritParams get_
#' @inheritParams base::unlink
#' @family cache_ 
#' @returns Null. 
#' @name cache_
NULL

#### link_ ####
#' @title Link functions
#' 
#' @description
#' Functions to merge data resources.
#' @inheritParams main_
#' @inheritParams get_
#' @inheritParams map_
#' @inheritParams to_
#' @inheritParams filter_
#' @family link_ 
#' @returns Merged data.
#' 
#' @name link_
NULL



