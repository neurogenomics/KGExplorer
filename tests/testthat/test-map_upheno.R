skip_if_offline()
test_that("map_upheno works", {
  
  library(ggplot2) # required step due to bug in ggdist

  run_tests <- function(res){
    testthat::expect_true(methods::is(res$data,"data.table"))
    for(x in res$plots){
      testthat::expect_true(methods::is(x,"gg") ||
                              methods::is(x,"Heatmap"))
    }
  }
  hp <- get_ontology('hp')
  terms <- hp@terms[seq(1000)]
  res <- map_upheno(force_new = FALSE,
                    # pheno_map_method="upheno",
                    terms = terms)
  run_tests(res)

  #### Use cached data and filter by HPO terms
  res <- map_upheno(force_new = FALSE,
                    terms = terms)
  run_tests(res)
})
