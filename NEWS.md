# KGExplorer 0.99.06

## Bug fixes
* `get_opentargets`: Limit threads and files to import in example.
* Merge fixes by @HDash https://github.com/neurogenomics/KGExplorer/pull/2
* `dt_to_kg`: fix reference to graph `g`.

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
