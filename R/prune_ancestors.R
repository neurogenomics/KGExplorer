#' Prune ancestor
#'
#' Prune redundant ancestral terms from a \link[data.table]{data.table}.
#' @export
#' @param dat A \link[data.table]{data.table} with a column of ontology terms.
#' @param id_col The name of the column containing ontology term IDs.
#' @inheritParams filter_
#' @examples
#' dat <- data.table::data.table(hpo_id=c("HP:0000001","HP:0000002","HP:0000003"),
#'                              name=c("term1","term2","term3"))
#' ont <- get_ontology("hp")
#' dat2 <- prune_ancestors(dat,id_col="hpo_id",ont=ont)
prune_ancestors <- function(dat,
                            id_col,
                            ont){
  messager("Pruning ancestors.")
  force(dat)
  force(id_col)
  force(ont)
  ids <- unique(dat[[id_col]])
  pruned_ids <- lapply(ids, function(x){
    if(!x %in% ont@terms) {
      messager(x,"is not in the ontology. Returning NULL.")
      return(NULL)
    }
    descendants <- simona::dag_offspring(dag = ont,
                                         term = x, 
                                         in_labels = FALSE)
    if(sum(descendants %in% ids) > 0){
      return(NULL)
    } else {
      return(x)
    }
  }) |> unlist()
  messager(length(pruned_ids),"/",length(ids),"terms were kept after pruning.")
  return(dat[get(id_col) %in% pruned_ids])
}
