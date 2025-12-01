# KGExplorer 0.99.10

## Bug fixes
* `get_ols_options`: Fix due to changes in rols namespace: 
  https://github.com/lgatto/rols/blob/master/NEWS.md#rols-299
* `get_ontology`: Add `tryCatch` as a safeguard against `rols` bugs.  
* `get_tdd`: Update TTD downloads domain name.
* Fix minor Roxygen warnings.
* Install `rols` from GitHub directly since maintainer is not fixing it on Bioc.

# KGExplorer 0.99.09

## Bug fixes
* *test-map_ontology_terms.R*: Fixed expected number of NA CL names.

## New features
* Remove `downloadR` dependency as it was causing unnecessary complications.

# KGExplorer 0.99.08

## Bug fixes
* `map_upheno_data`: 
  - Updated Monarch FTP endpoint 
  `subdir="latest/tsv/all_associations/"` --> `subdir="monarch-kg/latest/tsv/all_associations/"`
  - Remove "monarch" option for now (broken due to files being deleted on Monarch FTP server)
* `get_monarch`: Fix example
* `map_upheno`: Fix plotting error

# KGExplorer 0.99.07

## New features
* `get_ontology`: Saves files with release version control.

# KGExplorer 0.99.06

## Bug fixes
* `get_opentargets`: Limit threads and files to import in example.
* Merge fixes by @HDash https://github.com/neurogenomics/KGExplorer/pull/2
* `dt_to_kg`: fix reference to graph `g`.
* `get_hpo`
  - Remove ported function to avoid conflicting namespaces. 
    Instead, simply use `get_ontology("hpo")`

# KGExplorer 0.99.05

## New features
* `get_hpo`
  - Port function from `HPOExplorer` package to prevent circular dependency.

## Bug fixes
* `DESCRIPTION`
  - Update remote for `monarchr`.
* Tests
  - Add `skip_if_offline` to tests that (may) require internet access.
* `ontology_to`
  - `igraph::as_adj` (deprecated) -> `igraph::as_adjacency_matrix`.

# KGExplorer 0.99.04

## Bug fixes
* `test-get_ontology_levels`
  - Check for range rather than fixed values.
* `filter_ontology`
  - Move `terms` processing block to after check for character, as appropriate.
* `get_ontology_dict`
  - Add error handling for missing `alternative_terms` when
  `include_alternative_terms=TRUE`.
* `plot_ontology_heatmap`
  - Fix default value for argument `annot`-- cast one@elementMetadata to
  data.frame first.
* `prune_ancestors`
  - Add value for argument `id_col` in example.
* `set_cores`
  - Reduce workers during `R CMD CHECK` if required.

# KGExplorer 0.99.03

## New features

* `map_ontology_terms`: 
  - Can now recognize `alternative_terms` slot in simona object (https://github.com/jokergoo/simona/issues/6)
  - Automatically uses "short_id" when "id" is set, as the "id" column can sometimes contain long ID formats (http://purl.obolibrary.org/obo/MONDO_0100229 vs. MONDO:0100229)

# KGExplorer 0.99.01

## Bug fixes

- Fix `get_ontology_dict` data.table construction.

# KGExplorer 0.99.0

## New features
 
* Added a `NEWS.md` file to track changes to the package.

## Bug fixes

* Removed Dockerfile and switched to using `rworkflows`.
