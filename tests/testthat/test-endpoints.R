skip_on_cran()
skip_if_offline()

test_that("Testing if the `Ensembl API` functions work correctly", {

  # -------------------------------------------------------- #
  ## Comparative Genomics ====

  test_that("`get_cafe_genetree_by_id` works", {
    result <- get_cafe_genetree_by_id("ENSGT00390000003602")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_cafe_genetree_by_id(), "The 'id' parameter is required")
  })

  test_that("`get_cafe_genetree_by_symbol` works", {
    result <- get_cafe_genetree_by_symbol("homo_sapiens", "BRCA2")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_cafe_genetree_by_symbol("homo_sapiens"), "Both 'species' and 'symbol' parameters are required")
  })

  test_that("`get_cafe_genetree_by_species_id` works", {
    result <- get_cafe_genetree_by_species_id("homo_sapiens", "ENST00000380152")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_cafe_genetree_by_species_id("homo_sapiens"), "Both 'species' and 'id' parameters are required")
  })

  test_that("`get_genetree_by_id` works", {
    result <- get_genetree_by_id("ENSGT00390000003602")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_genetree_by_id(), "The 'id' parameter is required")
  })

  test_that("`get_genetree_by_symbol` works", {
    result <- get_genetree_by_symbol("homo_sapiens", "BRCA2")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_genetree_by_symbol("homo_sapiens"), "Both 'species' and 'symbol' parameters are required")
  })

  test_that("`get_genetree_by_species_id` works", {
    result <- get_genetree_by_species_id("homo_sapiens", "ENST00000380152")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_genetree_by_species_id("homo_sapiens"), "Both 'species' and 'id' parameters are required")
  })

  test_that("`get_alignment_by_region` works", {
    expect_warning(get_alignment_by_region("homo_sapiens", "3:1000-2000"), "This function is stil under-develop")
    # expect_error(get_alignment_by_region("homo_sapiens"), "Both 'species' and 'region' parameters are required")
  })

  test_that("`get_homology_by_species_id` works", {
    result <- get_homology_by_species_id("homo_sapiens", "ENSG00000157764")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_homology_by_species_id("homo_sapiens"), "Both 'species' and 'id' parameters are required")
  })

  test_that("`get_homology_by_symbol` works", {
    result <- get_homology_by_symbol("homo_sapiens", "BRCA2")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_homology_by_symbol("homo_sapiens"), "Both 'species' and 'symbol' parameters are required")
  })

  # -------------------------------------------------------- #
  ## Cross References ====

  test_that("`get_xrefs_by_symbol` works", {
    result <- get_xrefs_by_symbol("homo_sapiens", "BRCA2")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_xrefs_by_symbol("homo_sapiens"), "Both 'species' and 'symbol' parameters are required")
  })

  test_that("`get_xrefs_by_id` works", {
    result <- get_xrefs_by_id("ENSG00000157764")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_xrefs_by_id(), "The 'id' parameter is required")
  })

  test_that("`get_xrefs_by_name` works", {
    result <- get_xrefs_by_name("homo_sapiens", "P38398")
    expect_type(result, "list")
    expect_equal(result[[1]]$status_code, 200)
    expect_error(get_xrefs_by_name("homo_sapiens"), "Both 'species' and 'name' parameters are required")
  })

  # -------------------------------------------------------- #
  ## Information ====
  ## TO DO

  # -------------------------------------------------------- #
  ## Linkage Disequilibrium ====

  test_that("get_ld_by_variant works", {
    result <- get_ld_by_variant(species = "homo_sapiens", id = "rs56116432",
                                population_name = "1000GENOMES:phase_3:KHV")

    expect_type(result, "list")
    expect_true(!is.null(result))
    expect_true(length(result) > 0)
  })

  test_that("get_ld_by_variant handles missing parameters", {
    expect_error(get_ld_by_variant(id = "rs56116432", population_name = "1000GENOMES:phase_3:KHV"),
                 "'species', 'id', and 'population_name' parameters are all required.")
    expect_error(get_ld_by_variant(species = "homo_sapiens",population_name = "1000GENOMES:phase_3:KHV"),
                 "'species', 'id', and 'population_name' parameters are all required.")
  })

  test_that("get_pairwise_ld_values works", {
    result <- get_pairwise_ld_values(species = "homo_sapiens",
                                     id1 = "rs6792369",id2 = "rs1042779")

    expect_type(result, "list")
    expect_true(!is.null(result))
    expect_true(length(result) > 0)
  })

  test_that("get_pairwise_ld_values handles missing parameters", {
    expect_error(get_pairwise_ld_values(id1 = "rs6792369", id2 = "rs1042779"),
                 "'species', 'id1', and 'id2' parameters are all required.")
    expect_error(get_pairwise_ld_values(species = "homo_sapiens", id1 = "rs6792369"),
                 "'species', 'id1', and 'id2' parameters are all required.")
  })

  test_that("get_ld_values_by_region works", {
    result <- get_ld_values_by_region(species = "homo_sapiens",
                                      region = "6:25837556..25843455",
                                      population_name = "1000GENOMES:phase_3:KHV")

    expect_type(result, "list")
    expect_true(!is.null(result))
    expect_true(length(result) > 0)
  })

  test_that("get_ld_values_by_region handles missing parameters", {
    expect_error(get_ld_values_by_region(region = "6:25837556..25843455", population_name = "1000GENOMES:phase_3:KHV"),
                 "'species', 'region', and 'population_name' parameters are all required.")
    expect_error(get_ld_values_by_region(species = "homo_sapiens", population_name = "1000GENOMES:phase_3:KHV"),
                 "'species', 'region', and 'population_name' parameters are all required.")
  })

  ## test `get`
  id <- "ENSGT00390000003602"
  response <- get(res = "/cafe/genetree/id/{id}", id = id,
    .headers = req_headers(content_type = "application/json")
  )
  expect_equal(response[[1]]$status_code, 200)

})
