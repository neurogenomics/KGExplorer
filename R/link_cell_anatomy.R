link_cell_anatomy <- function(){
  subject_category <- NULL;
  
  g <- get_monarch_kg()
  from_categories = paste0("biolink:",
                           c("GrossAnatomicalStructure",
                             "AnatomicalEntity")
  )
  to_categories <- "biolink:Cell" 
  g2 <- filter_kg(g, 
                  from_categories = from_categories,
                  to_categories = to_categories)
  g2 <- filter_graph(g2,
                     node_filters = list(namespace=c("UBERON","CL")))
  dt <- graph_to_dt(g = g2)
  dt[subject_category=="biolink:AnatomicalEntity"]$subject_label|> unique()|> sort()
  dt[subject_category=="biolink:GrossAnatomicalStructure"]$subject_label|> unique()|> sort()
  uberon <- get_ontology("uberon", 
                         terms = unique(dt[subject_category=="biolink:AnatomicalEntity"]$subject),
                         add_ancestors = 1)
  anatomy_map <- rbind(
    merge(dt, 
          uberon@elementMetadata, 
          by.x="object",
          by.y="id"),
    merge(dt, 
          uberon@elementMetadata, 
          by.x="subject",
          by.y="id")
  )
  # cl <- get_ontology("cl", add_ancestors = 1)
  # dend <- ontology_to(cl,
  #                     terms = unique(dt[subject_category=="biolink:Cell"]$subject),
  #                     to="dendrogram")
  
  # unique(dt,subjc)
}
