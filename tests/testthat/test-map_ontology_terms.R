skip_if_offline()
test_that("map_ontology_terms works", {

  
  run_tests <- function(ont, terms){
    term_names <- map_ontology_terms(ont=ont, terms=terms)
    term_ids <- map_ontology_terms(ont=ont, terms=terms, to="id") 
    testthat::expect_true(
      length(term_names)==length(terms)
    )
    testthat::expect_true(
      length(term_ids)==length(terms)
    )
    testthat::expect_true(
      all(names(term_names)==terms)
    ) 
    testthat::expect_true(
      all(names(term_ids)==terms)
    ) 
  }
  #### HPO ####
  ont <- get_ontology("hp")
  terms <- c("Focal motor seizure",
              "Focal MotoR SEIzure",
              "HP:0000002","HP:0000003")
  run_tests(ont = ont, terms = terms)
  
  #### CL ####
  cl <-  get_ontology("cl", )
  testthat::expect_lte(nrow(subset(cl@elementMetadata, is.na(name))), 20)
  terms <- cl@terms
  run_tests(ont = ont, terms = terms) 
})
