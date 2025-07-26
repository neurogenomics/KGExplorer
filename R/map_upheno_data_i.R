map_upheno_data_i <- function(pheno_map_method,
                              gene_map_method,
                              keep_nogenes,
                              fill_scores,
                              terms,
                              agg){
  hgnc <- gene_label1 <- subject <- subject_taxon_label <-
    hgnc_label <- n_phenotypes <- n_genes_intersect <-
    prop_intersect <- p1 <- p2 <- db1 <- db2 <- id1 <- id2 <-
    equivalence_score <- subclass_score  <-  hgnc_label1 <-
    hgnc_label2 <- hgnc_id2 <- gene <- gene_label1 <- gene_label2 <-
    n_genes_db1 <- object <- gene_label <- db <- .  <-
    n_genes_db2 <- subject_taxon_label1 <- subject_taxon_label2 <-
    phenotype_genotype_score <- equivalence_score <- NULL;

  pheno_map_method <- pheno_map_method[1]
  gene_map_method <- gene_map_method[1]
  messager(paste0("map_upheno_data: pheno_map_method=",
                  shQuote(pheno_map_method)))
  #### Import data ####
  ## Cross-species phenotype map
  {
    if(pheno_map_method=="upheno"){
      pheno_map <- get_upheno(file = "bestmatches")
      names(pheno_map) <-gsub("^subject","id1",names(pheno_map))
      names(pheno_map) <-gsub("^object","id2",names(pheno_map))
      pheno_map[,db1:=gsub("*:.*","",basename(id1))]
      
      
    } 
    # else if(pheno_map_method=="monarch"){
    #   #### NOTE: DEPRECATED FOR THE TIME BEING ####
    # 
    #   hpo <- get_ontology(name = "hpo")
    #   out <- monarchr::monarch_search(query = NULL,
    #                                   category = "biolink:PhenotypicFeature",
    #                                   limit = 10)
    #   
    #   
    #   pheno_map <- get_monarch(queries = "phenotype_to_phenotype") |>
    #     data.table::setnames(c("label_x","label_y"),c("label1","label2"))
    #   
    #   pheno_map[,id1:=gsub("_",":",basename(p1))
    #   ][,id2:=gsub("_",":",basename(p2))]
    #   pheno_map[,db1:=gsub("*_.*","",basename(p1))
    #   ][,db2:=gsub("*_.*","",basename(p2))]
    #   pheno_map[,equivalence_score:=NA][,subclass_score:=NA]
    # }
    #### Filter data ####
    if(!is.null(terms)){
      pheno_map <- pheno_map[id1 %in% terms,]
      if(nrow(pheno_map)==0){
        stop("No terms found in pheno_map")
      }
    }
  }

  ## Gene-phenotype associations across 8 species
  {
    genes <- get_monarch(maps = list(c("phenotypic_feature","gene"),
                                     c("disease_or_phenotypic_feature","gene",
                                       "disease"="causal_gene",
                                       "disease"="correlated_gene")),
                         rbind=TRUE)
    data.table::setkeyv(genes,"object")
    messager("Unique species with genes:",
             data.table::uniqueN(genes$subject_taxon_label))
    ## Create an ID-label map for each phenotype
    genes_map <- genes[,c("object","object_label",
                          "subject_taxon","subject_taxon_label")] |> unique()
    genes_map <- genes_map[,.SD[1], by="object"]
    add_db(genes_map,
           input_col = "object",
           output_col = "db")
    ## Create an db-species map for each Ontology
    species_map <- genes_map[,.SD[1], keyby="db"][,.(db,subject_taxon_label)]
  }

  #### Map non-human genes onto human orthologs ####
  {
    genes_homol <- map_genes_monarch(dat=genes,
                                     gene_col="subject")
    names(genes_homol) <- gsub("subject","gene",names(genes_homol))
    messager(data.table::uniqueN(genes_homol$gene_taxon_label),"/",
             data.table::uniqueN(genes$subject_taxon_label),
             "species remain after cross-species gene mapping.")
  }

  #### Map non-human phenotypes onto human phenotypes ####
  #### Merge nonhuman ontology genes with human HPO genes ####
  {
    messager("Merging nonhuman ontology genes with human HPO genes.")
    pheno_map_genes <- data.table::merge.data.table(
      pheno_map,
      genes_homol,
      by.x="id1",
      by.y="object",
      all.x = keep_nogenes,
      allow.cartesian = TRUE) |>
      #### Merge nonhuman ontology genes with human HPO genes ####
    data.table::merge.data.table(
      genes_homol,
      by.x="id2",
      by.y="object",
      all.y = keep_nogenes,
      suffixes = c(1,2),
      allow.cartesian = TRUE
    )
    pheno_map_genes[,db2:=id2_db]
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
    if(isFALSE(agg)) return(pheno_map_genes_match)
    pheno_map_genes_match <-
      pheno_map_genes_match[,
                            list(n_genes_intersect=data.table::uniqueN(hgnc2)),
                            by=c("id1","db1","object_label1","n_genes_db1",
                                 "id2","db2","object_label2","n_genes_db2",
                                 "gene_taxon1","gene_taxon_label1",
                                 "gene_taxon2","gene_taxon_label2",
                                 "equivalence_score","subclass_score")
      ] |>
      data.table::setorderv("n_genes_intersect",-1)
    pheno_map_genes_match[,n_phenotypes:=data.table::uniqueN(id1),
                          by=c("db1","db2",
                               "gene_taxon1","gene_taxon2",
                               "gene_taxon_label1","gene_taxon_label2"
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
