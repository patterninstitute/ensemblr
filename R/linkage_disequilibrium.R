ld_tbl <- function(
  species_name = character(),
  population = character(),
  variant_id1 = character(),
  variant_id2 = character(),
  d_prime = double(),
  r_squared = double()
) {
  tbl <- tibble::tibble(
    species_name = species_name,
    population = population,
    variant_id1 = variant_id1,
    variant_id2 = variant_id2,
    d_prime = d_prime,
    r_squared = r_squared
  )
  return(tbl)
}

json_list_to_ld_tbl <- function(species_name, json_list) {

  tbl <- ld_tbl(
    species_name = species_name,
    population = purrr::pluck(json_list, 'population_name', .default = NA_character_),
    variant_id1 = purrr::pluck(json_list, 'variation1', .default = NA_character_),
    variant_id2 = purrr::pluck(json_list, 'variation2', .default = NA_character_),
    d_prime = purrr::pluck(json_list, 'd_prime', .default = NA_real_),
    r_squared = purrr::pluck(json_list, 'r2', .default = NA_real_)
  )

  # Drop rows if all columns except species_name are NA
  return(tidyr::drop_na(tbl, -species_name))
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


#' Get linkage disequilibrium data for variants
#'
#' Gets linkage disequilibrium data for variants from Ensembl REST API. There
#' are three ways to query, either by:
#' \describe{
#' \item{A genomic window centred on a
#' variant:}{\code{get_ld_variants_by_window(variant_id, genomic_window_size,
#' ...)}}
#' \item{A pair of variants:}{\code{get_ld_variants_by_pair(variant_id1,
#' variant_id2, ...)}}
#' \item{A genomic range:}{\code{get_ld_variants_by_range(genomic_range, ...)}}
#' }
#'
#' @param variant_id A variant identifier, e.g., \code{'rs123'}. This argument
#'   is to be used with function \code{get_ld_variants_by_window()}. Note that this
#'   argument is not the same as \code{variant_id1} or \code{variant_id2}, to be
#'   used with function \code{get_ld_variants_by_pair}.
#' @param genomic_window_size An integer vector specifying the genomic window
#'   size in kilobases (kb) around the variant indicated in \code{variant_id}.
#'   This argument is to be used with function
#'   \code{get_ld_variants_by_window()}. At the moment, the Ensembl REST API
#'   does not allow values greater than 500kb. A window size of 500 means
#'   looking 250kb upstream and downstream the variant passed as
#'   \code{variant_id}. The minimum value for this argument is \code{1L}, not
#'   \code{0L}.
#' @param variant_id1 The first variant of a pair of variants. Used with
#'   \code{variant_id2}. Note that this argument is not the same as
#'   \code{variant_id}. This argument is to be used with function
#'   \code{get_ld_variants_by_pair()}.
#' @param variant_id2 The second variant of a pair of variants. Used with
#'   \code{variant_id1}. Note that this argument is not the same as
#'   \code{variant_id}. This argument is to be used with function
#'   \code{get_ld_variants_by_pair()}.
#' @param genomic_range Genomic range formatted as a string
#'   \code{"chr:start..end"}, e.g., \code{"X:1..10000"}. Check function
#'   \code{\link[ensemblr]{genomic_range}} to easily create these ranges from
#'   vectors of start and end positions. This argument is to be used with
#'   function \code{get_ld_variants_by_range()}.
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'} (Goat).
#' @param population Population for which to compute linkage disequilibrium. TODO.
#' @param d_prime \eqn{D'} is a measure of linkage disequilibrium.
#'   \code{d_prime} defines a cut-off threshold: only variants whose
#'   \eqn{D' \ge }\code{d_prime} are returned.
#' @param r_squared \eqn{r^2} is a measure of linkage disequilibrium.
#'   \code{r_squared} defines a cut-off threshold: only variants whose
#'   \eqn{r^2 \ge }\code{r_squared} are returned.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of 6 variables:
#' \describe{
#'   \item{\code{species_name}}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{\code{population}}{Population for which to compute linkage disequilibrium.}
#'   \item{\code{variant_id1}}{First variant identifier.}
#'   \item{\code{variant_id2}}{Second variant identifier.}
#'   \item{\code{d_prime}}{\eqn{D'} between the two variants.}
#'   \item{\code{r_squared}}{\eqn{r^2} between the two variants.}
#' }
#' @export
#' @name get_ld_variants_by_by_window
#' @examples
#' # Retrieve variants in LD by a window size of 1kb:
#' # 1kb: 500 bp upstream and 500 bp downstream of variant.
#' get_ld_variants_by_window('rs123', genomic_window_size = 1L)
#'
#' # Retrieve LD measures for pairs of variants:
#' get_ld_variants_by_pair(
#'   variant_id1 = c('rs123', 'rs35439278'),
#'   variant_id2 = c('rs122', 'rs35174522')
#' )
#'
#' # Retrieve variants in LD within a genomic range
#' get_ld_variants_by_range('7:100000..100500')

#' @export
#' @rdname get_ld_variants_by_window
get_ld_variants_by_window <- function(variant_id,
                                   genomic_window_size = 500L,
                                   species_name = 'homo_sapiens',
                                   population = '1000GENOMES:phase_3:CEU',
                                   d_prime = 0.0,
                                   r_squared = 0.0,
                                   verbose = FALSE,
                                   warnings = TRUE,
                                   progress_bar = TRUE) {
  # Assert variant_id argument.
  assert_variant_id(variant_id)
  # Assert genomic_window_size argument.
  assert_genomic_window_size(genomic_window_size)
  # Assert species_name argument.
  assert_species_name(species_name)
  # Assert population argument.
  assert_population(population)
  # Assert d_prime argument.
  assert_d_prime(d_prime)
  # Assert r_squared argument.
  assert_r_squared(r_squared)
  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  error_msg <- glue::glue(
    'All arguments must have consistent lengths, ',
    'only values of length one are recycled:\n',
    '* Length of `variant_id`: {length(variant_id)}\n',
    '* Length of `genomic_window_size`: {length(genomic_window_size)}\n',
    '* Length of `species_name`: {length(species_name)}\n',
    '* Length of `population`: {length(population)}\n',
    '* Length of `d_prime`: {length(d_prime)}\n',
    '* Length of `r_squared`: {length(r_squared)}'
  )
  if (!are_vec_recyclable(variant_id,
                     genomic_window_size,
                     species_name,
                     population,
                     d_prime,
                     r_squared))
    rlang::abort(error_msg)

  recycled_args <- vctrs::vec_recycle_common(variant_id,
                            genomic_window_size,
                            species_name,
                            population,
                            d_prime,
                            r_squared)
  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c(
    'variant_id',
    'genomic_window_size',
    'species_name',
    'population',
    'd_prime',
    'r_squared')

  resource_urls <- glue::glue(
    '/ld/',
    '{recycled_args$species_name}/',
    '{recycled_args$variant_id}/',
    '{recycled_args$population}?',
    'window_size={recycled_args$genomic_window_size};',
    'd_prime={recycled_args$d_prime};',
    'r2={recycled_args$r_squared}'
    )

  # Usually we'd use purrr::map here but we opted for plyr::llply
  # for a no frills alternative with progress bar support.
  progress <- dplyr::if_else(progress_bar, 'text', 'none')
  responses <- plyr::llply(
    .data = resource_urls,
    .fun = request,
    verbose = verbose,
    warnings = warnings,
    .progress = progress)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ .x$status == 'OK')

  # If none of the responses were successful then return an empty linkage
  # disequilibrium tibble.
  if (rlang::is_empty(responses_ok)) return(ld_tbl())

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_ld_tbl(
        species_name = recycled_args$species_name[.y],
        json_list = .x$content
      )
    )
  )

}

#' @export
#' @rdname get_ld_variants_by_window
get_ld_variants_by_pair <- function(variant_id1,
                                 variant_id2,
                                 species_name = 'homo_sapiens',
                                 population = '1000GENOMES:phase_3:CEU',
                                 d_prime = 0.0,
                                 r_squared = 0.0,
                                 verbose = FALSE,
                                 warnings = TRUE,
                                 progress_bar = TRUE) {

  # Assert variant_id1 argument.
  assert_variant_id(variant_id1)
  # Assert variant_id2 argument.
  assert_variant_id(variant_id2)
  # Assert species_name argument.
  assert_species_name(species_name)
  # Assert population argument.
  assert_population(population)
  # Assert d_prime argument.
  assert_d_prime(d_prime)
  # Assert r_squared argument.
  assert_r_squared(r_squared)
  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  error_msg <- glue::glue(
    'All arguments must have consistent lengths, ',
    'only values of length one are recycled:\n',
    '* Length of `variant_id1`: {length(variant_id1)}\n',
    '* Length of `variant_id2`: {length(variant_id2)}\n',
    '* Length of `species_name`: {length(species_name)}\n',
    '* Length of `population`: {length(population)}\n',
    '* Length of `d_prime`: {length(d_prime)}\n',
    '* Length of `r_squared`: {length(r_squared)}'
  )
  if (!are_vec_recyclable(variant_id1,
                          variant_id2,
                          species_name,
                          population,
                          d_prime,
                          r_squared))
    rlang::abort(error_msg)

  recycled_args <- vctrs::vec_recycle_common(variant_id1,
                                             variant_id2,
                                             species_name,
                                             population,
                                             d_prime,
                                             r_squared)

  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c(
    'variant_id1',
    'variant_id2',
    'species_name',
    'population',
    'd_prime',
    'r_squared')

  resource_urls <- glue::glue(
    '/ld/',
    '{recycled_args$species_name}/',
    'pairwise/',
    '{recycled_args$variant_id1}/',
    '{recycled_args$variant_id2}?',
    'population_name={recycled_args$population};',
    'd_prime={recycled_args$d_prime};',
    'r2={recycled_args$r_squared}'
  )

  # Usually we'd use purrr::map here but we opted for plyr::llply
  # for a no frills alternative with progress bar support.
  progress <- dplyr::if_else(progress_bar, 'text', 'none')
  responses <- plyr::llply(
    .data = resource_urls,
    .fun = request,
    verbose = verbose,
    warnings = warnings,
    .progress = progress)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ .x$status == 'OK')

  # If none of the responses were successful then return an empty linkage
  # disequilibrium tibble.
  if (rlang::is_empty(responses_ok)) return(ld_tbl())

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_ld_tbl(
        species_name = recycled_args$species_name[.y],
        json_list = .x$content
      )
    )
  )

}

#' @export
#' @rdname get_ld_variants_by_window
get_ld_variants_by_range <- function(genomic_range,
                                  species_name = 'homo_sapiens',
                                  population = '1000GENOMES:phase_3:CEU',
                                  d_prime = 0.0,
                                  r_squared = 0.0,
                                  verbose = FALSE,
                                  warnings = TRUE,
                                  progress_bar = TRUE) {
  # Assert genomic_range argument.
  assert_genomic_range(genomic_range)
  # Assert species_name argument.
  assert_species_name(species_name)
  # Assert population argument.
  assert_population(population)
  # Assert d_prime argument.
  assert_d_prime(d_prime)
  # Assert r_squared argument.
  assert_r_squared(r_squared)
  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  error_msg <- glue::glue(
    'All arguments must have consistent lengths, ',
    'only values of length one are recycled:\n',
    '* Length of `genomic_range`: {length(genomic_range)}\n',
    '* Length of `species_name`: {length(species_name)}\n',
    '* Length of `population`: {length(population)}\n',
    '* Length of `d_prime`: {length(d_prime)}\n',
    '* Length of `r_squared`: {length(r_squared)}'
  )
  if (!are_vec_recyclable(genomic_range,
                          species_name,
                          population,
                          d_prime,
                          r_squared))
    rlang::abort(error_msg)

  recycled_args <- vctrs::vec_recycle_common(genomic_range,
                                             species_name,
                                             population,
                                             d_prime,
                                             r_squared)

  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c(
    'genomic_range',
    'species_name',
    'population',
    'd_prime',
    'r_squared')

  resource_urls <- glue::glue(
    '/ld/',
    '{recycled_args$species_name}/',
    'region/',
    '{recycled_args$genomic_range}/',
    '{recycled_args$population}?',
    'd_prime={recycled_args$d_prime};',
    'r2={recycled_args$r_squared}'
  )

  # Usually we'd use purrr::map here but we opted for plyr::llply
  # for a no frills alternative with progress bar support.
  progress <- dplyr::if_else(progress_bar, 'text', 'none')
  responses <- plyr::llply(
    .data = resource_urls,
    .fun = request,
    verbose = verbose,
    warnings = warnings,
    .progress = progress)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ .x$status == 'OK')

  # If none of the responses were successful then return an empty linkage
  # disequilibrium tibble.
  if (rlang::is_empty(responses_ok)) return(ld_tbl())

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_ld_tbl(
        species_name = recycled_args$species_name[.y],
        json_list = .x$content
      )
    )
  )

}
