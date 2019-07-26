assert_division <- function(division) {

  if (
    !((rlang::is_character(division) &&
       all(division %in% (divisions <- get_divisions())))
    )
  ) {
    possible_values <- concatenate::cc_or(divisions, oxford = TRUE)
    msg <- glue::glue('`division` must be one or more Ensembl divisions:\n',
                      '{possible_values}.')
    rlang::abort(msg)
  }

  return(TRUE)
}

assert_variant_id <- function(variant_id) {

  # Is variant_id NULL?
  assertthat::assert_that(
    !rlang::is_null(variant_id),
    msg = '`variant_id` cannot be NULL.'
  )

  # Is variant_id empty?
  assertthat::not_empty(variant_id)

  # Is variant_id a character vector?
  assertthat::assert_that(
    rlang::is_character(variant_id),
    msg = '`variant_id` must be a character vector.'
  )

  # Does variant_id contain NAs?
  assertthat::assert_that(assertthat::noNA(variant_id))

  return(TRUE)
}

assert_genomic_window_size <- function(genomic_window_size) {

  # Is genomic_window_size NULL?
  assertthat::assert_that(
    !rlang::is_null(genomic_window_size),
    msg = '`genomic_window_size` cannot be NULL.'
  )

  # Is genomic_window_size an integer vector?
  assertthat::assert_that(
    rlang::is_integer(genomic_window_size),
    msg = '`genomic_window_size` must be an integer vector.'
  )

  # Does genomic_window_size contain NAs?
  assertthat::assert_that(assertthat::noNA(genomic_window_size))

  # Is genomic_window_size in [1, 500]?
  assertthat::assert_that(
    all(dplyr::between(genomic_window_size, 1L, 500L)),
    msg = '`genomic_window_size` contains values outside the range [1, 500].'
  )

  return(TRUE)
}

assert_species_name <- function(species_name) {

  # Is species_name NULL?
  assertthat::assert_that(
    !rlang::is_null(species_name),
    msg = '`species_name` cannot be NULL.'
  )

  # Is species_name empty?
  assertthat::not_empty(species_name)

  # Is species_name a character vector?
  assertthat::assert_that(
    rlang::is_character(species_name),
    msg = '`species_name` must be a character vector.'
  )

  # Does species_name contain NAs?
  assertthat::assert_that(assertthat::noNA(species_name))

  return(TRUE)
}

assert_population <- function(population) {

  # Is population NULL?
  assertthat::assert_that(
    !rlang::is_null(population),
    msg = '`population` cannot be NULL.'
  )

  # Is population empty?
  assertthat::not_empty(population)

  # Is population a character vector?
  assertthat::assert_that(
    rlang::is_character(population),
    msg = '`population` must be a character vector.'
  )

  # Does population contain NAs?
  assertthat::assert_that(assertthat::noNA(population))

  return(TRUE)
}

assert_d_prime <- function(d_prime) {

  # Is d_prime NULL?
  assertthat::assert_that(
    !rlang::is_null(d_prime),
    msg = '`d_prime` cannot be NULL.'
  )

  # Is d_prime a double vector?
  assertthat::assert_that(
    rlang::is_double(d_prime),
    msg = '`d_prime` must be a double vector.'
  )

  # Does d_prime contain NAs?
  assertthat::assert_that(assertthat::noNA(d_prime))

  # Is d_prime in [0, 1]?
  assertthat::assert_that(
    all(dplyr::between(d_prime, 0, 1)),
    msg = '`d_prime` contains values outside the range [0, 1].'
  )

  return(TRUE)
}

assert_r_squared <- function(r_squared) {

  # Is r_squared NULL?
  assertthat::assert_that(
    !rlang::is_null(r_squared),
    msg = '`r_squared` cannot be NULL.'
  )

  # Is r_squared a double vector?
  assertthat::assert_that(
    rlang::is_double(r_squared),
    msg = '`r_squared` must be a double vector.'
  )

  # Does r_squared contain NAs?
  assertthat::assert_that(assertthat::noNA(r_squared))

  # Is r_squared in [0, 1]?
  assertthat::assert_that(
    all(dplyr::between(r_squared, 0, 1)),
    msg = '`r_squared` contains values outside the range [0, 1].'
  )

  return(TRUE)
}

assert_genomic_range <- function(genomic_range) {

  # Is genomic_range NULL?
  assertthat::assert_that(
    !rlang::is_null(genomic_range),
    msg = '`genomic_range` cannot be NULL.'
  )

  # Is genomic_range empty?
  assertthat::not_empty(genomic_range)

  # Is genomic_range a character vector?
  assertthat::assert_that(
    rlang::is_character(genomic_range),
    msg = '`genomic_range` must be a character vector.'
  )

  # Does genomic_range contain NAs?
  assertthat::assert_that(assertthat::noNA(genomic_range))

  genomic_range_lgl <- is_genomic_range(genomic_range)
  assertthat::assert_that(
    all(genomic_range_lgl),
    msg = glue::glue("The following are not well formatted genomic ranges:',
                     ' {concatenate::cc_and(genomic_range[genomic_range_lgl])}."))

  return(TRUE)
}
