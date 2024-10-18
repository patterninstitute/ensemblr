skip_on_cran()
skip_if_offline()

test_that("Testing if the `Ensembl API` functions work correctly", {

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

})
