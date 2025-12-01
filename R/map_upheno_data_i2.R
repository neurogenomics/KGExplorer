map_upheno_data_i2 <- function(pheno_map_method,
                              gene_map_method,
                              keep_nogenes,
                              fill_scores,
                              terms){
  hgnc <- gene_label1 <- subject <- subject_taxon_label <-
    hgnc_label <- n_phenotypes <- n_genes_intersect <-
    prop_intersect <- p1 <- p2 <- db1 <- db2 <- id1 <- id2 <-
    equivalence_score <- subclass_score  <-  hgnc_label1 <-
    hgnc_label2 <- hgnc_id2 <- gene <- gene_label1 <- gene_label2 <-
    n_genes_db1 <- object <- gene_label <- db <- .  <-
    n_genes_db2 <- subject_taxon_label1 <- subject_taxon_label2 <-
    phenotype_genotype_score <- equivalence_score <- subject_category <- 
    object_category <- category <- taxon_label <- gene_taxon_label1 <- 
    gene_taxon_label2 <- NULL;
  
  pheno_map_method <- pheno_map_method[1]
  gene_map_method <- gene_map_method[1]
  messager(paste0("map_upheno_data: pheno_map_method=",
                  shQuote(pheno_map_method)))
  #### Import data ####
  ## Cross-species phenotype map
  {
    if(pheno_map_method=="upheno"){
      pheno_map <- get_upheno(file = "bestmatches")
      pheno_map[,subject_category:="phenotype"][,object_category:="phenotype"]
      pheno_map <- dt_to_graph(pheno_map) 
    } else if(pheno_map_method=="monarch"){
      pheno_map <- get_monarch("phenotype_to_phenotype") |>
        data.table::setnames(c("label_x","label_y"),c("label1","label2"))
      pheno_map[,id1:=gsub("_",":",basename(p1))
      ][,id2:=gsub("_",":",basename(p2))]
      pheno_map[,db1:=gsub("*_.*","",basename(p1))
      ][,db2:=gsub("*_.*","",basename(p2))]
      pheno_map[,equivalence_score:=NA][,subclass_score:=NA]
    }
    #### Filter data ####
    if(!is.null(terms)){
      pheno_map <- filter_graph(pheno_map,
                                node_filters=list(id=terms))
      if(length(pheno_map)==0){
        stop("No terms found in pheno_map.")
      }
    }
  }
  
  ## Gene-phenotype associations across 8 species
  {
    genes <- get_monarch(maps = list(c("phenotype","gene")),
                         rbind=TRUE)
    genes[,subject_category:="gene"][,object_category:="phenotype"]
    genes <- add_db(genes,
                    input_col = "subject")
    genes <- add_db(genes,
                    input_col = "object")
    genes <- dt_to_graph(dat = genes)
    ## Create an db-species map for each Ontology
    species_map <- graph_to_dt(genes,
                               what = "nodes")[category=="gene",.SD[1],
                                               keyby="db"][,.(db,taxon_label)]
  }
  
  #### Map non-human genes onto human orthologs ####
  {
    genes_homol <- map_genes_monarch(dat=genes,
                                     gene_col="subject") 
    nodes <- graph_to_dt(genes_homol,
                         what = "nodes") 
    # dat_homol <- genes_homol|>
    #   tidygraph::activate("nodes")|>
    #   tidygraph::filter(
    #     tidygraph::node_is_adjacent(to= nodes[,.I[category=="phenotype"]]) &
    #     tidygraph::node_is_adjacent(to= nodes[,.I[db %in% c("HGNC") ]])
    #     # tidygraph::node_is_connected(nodes[,.I[db %in% c("HGNC") ]], any=TRUE)
    #       # tidygraph::node_distance_to(nodes[,.I[db %in% c("HGNC") ]])<2
    #     )
    dat_homol <- genes_homol|>
      tidygraph::activate("edges")|>
      tidygraph::filter(
        tidygraph::edge_is_between(
          from= nodes[,.I[category=="phenotype"]],
          to= nodes[,.I[category=="gene" & db %in% c("HGNC") ]],
          ignore_dir=TRUE)
        )
    report_filter(g1=genes_homol,
                 g2=dat_homol,
                 cols = "taxon_label",
                 suffix= "species remain after cross-species gene mapping.")
  }
  
  #### Map non-human phenotypes onto human phenotypes ####
  #### Merge nonhuman ontology genes with human HPO genes ####
  {
    pheno_map_genes <- tidygraph::graph_join(pheno_map,
                                             dat_homol) 
    # g <- pheno_map_genes|> 
    #   tidygraph::activate(edges)|>
    #   tidygraph::sample_n(100)|> 
    #   tidygraph::activate(nodes)|>
    #   tidygraph::filter(!tidygraph::node_is_isolated())
    # plot_graph_visnetwork(g)
    ## Fill in missing species for those without gene data
    pheno_map_genes[
      is.na(gene_taxon_label1),
      gene_taxon_label1:=species_map[db1]$subject_taxon_label]
    pheno_map_genes[
      is.na(gene_taxon_label2),
      gene_taxon_label2:=species_map[db2]$subject_taxon_label]
    ## Add gene counts
    pheno_map_genes[,n_genes_db1:=data.table::uniqueN(gene_label1), by="id1"]
    pheno_map_genes[,n_genes_db2:=data.table::uniqueN(gene_label2), by="id2"]
    ## Report
    messager(data.table::uniqueN(pheno_map_genes$gene_taxon_label2),"/",
             data.table::uniqueN(genes_homol$gene_taxon_label),
             "species remain after cross-species phenotype mapping.")
    ## Remove
    # remove(genes_human,genes_nonhuman,pheno_map)
  }
  
  #### Count the number of overlapping genes
  {
    if(isFALSE(keep_nogenes)){
      pheno_map_genes_match <- pheno_map_genes[hgnc_label1==hgnc_label2,]
    } else {
      pheno_map_genes_match <- pheno_map_genes |> data.table::copy()
    }
    pheno_map_genes_match <-
      pheno_map_genes_match[,
                            list(n_genes_intersect=data.table::uniqueN(hgnc_id2)),
                            by=c("id1","db1","label1","n_genes_db1",
                                 "id2","db2","label2","n_genes_db2",
                                 "subject_taxon1","subject_taxon_label1",
                                 "subject_taxon2","subject_taxon_label2",
                                 "equivalence_score","subclass_score")
      ] |>
      data.table::setorderv("n_genes_intersect",-1)
    pheno_map_genes_match[,n_phenotypes:=data.table::uniqueN(id1),
                          by=c("db1","db2",
                               "subject_taxon1","subject_taxon2",
                               "subject_taxon_label1","subject_taxon_label2"
                          )]
    pheno_map_genes_match[,prop_intersect:=(n_genes_intersect/n_genes_db1)]
    ## Compute a score that captures both the phenotype mapping score and
    ## the poportional gene overlap score.
    pheno_map_genes_match[,phenotype_genotype_score:=data.table::nafill(
      (equivalence_score*prop_intersect)^(1/2),fill = 0)]
    # remove(pheno_map_genes)
  }
  ## Fill missing data
  if(!is.null(fill_scores)){
    data.table::setnafill(x = pheno_map_genes_match,
                          fill = fill_scores,
                          cols=c("equivalence_score","subclass_score"))
  }
  ## Check that the number of intersecting nonhuman ontology genes is always
  ## less than or equal to the number of total HPO genes.
  # pheno_map_genes_match[n_genes>n_genes_hpo,]
  return(pheno_map_genes_match)
}