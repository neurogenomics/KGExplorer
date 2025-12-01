#' @describeIn map_ map_
#' Map Medgen.
#' @inheritDotParams map_mondo
map_medgen <- function(dat,
                       input_col,
                       ...){
  source_id <- mondo_id <- definition <- DEF <- NULL;
  
  maps <- get_medgen_maps()
  map <- maps$map
  def <- maps$def
  # sort(table(map$source), decreasing = TRUE)
  #### Conform disease_id to MedGen format ####
  phenos <- map_mondo(dat = dat,
                      input_col = input_col,
                      ...)
  phenos[,source_id:=get(input_col)]
  phenos <- phenos[!is.na(get(input_col))]
  if(input_col=="disease_id"){
    phenos[,source_id:=gsub("OMIM:","",source_id)]
    phenos[,source_id:=gsub("ORPHANET:","Orphanet_",source_id)]
    phenos[,source_id:=ifelse(source_id %in% unique(map$source_id),
                              source_id, mondo_id)]
  }
  mapped <- data.table::merge.data.table(
    phenos,map,
    by="source_id",
    all.x = TRUE
  ) |>
    data.table::merge.data.table(def,
                                 by=c("CUI","source"),
                                 all.x = TRUE)
  data.table::uniqueN(phenos[!is.na(definition)]$hpo_id)
  data.table::uniqueN(grep("^HP:",map$source_id, value = TRUE))
  data.table::uniqueN(def[source=="HPO"]$CUI)
  data.table::uniqueN(mapped[!is.na(DEF)]$hpo_id)
}