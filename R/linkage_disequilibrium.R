ld_tbl <- function(
  species_name = character(),
  population = character(),
  variant_id1 = character(),
  variant_id2 = character(),
  r_squared = double(),
  d_prime = double()
) {
  tbl <- tibble::tibble(
    species_name = species_name,
    population = population,
    variant_id1 = variant_id1,
    variant_id2 = variant_id2,
    r_squared = r_squared,
    d_prime = d_prime
  )

  return(tbl)
}

json_list_to_ld_tbl <- function(species_name, json_list) {

  tbl <- ld_tbl(
    species_name = species_name,
    population = purrr::pluck(json_list, 'population_name', .default = NA_character_),
    variant_id1 = purrr::pluck(json_list, 'variation1', .default = NA_character_),
    variant_id2 = purrr::pluck(json_list, 'variation2', .default = NA_character_),
    r_squared = as.double(purrr::pluck(json_list, 'r2', .default = NA_real_)),
    d_prime = as.double(purrr::pluck(json_list, 'd_prime', .default = NA_real_))
  )

  # Drop rows if all columns except species_name are NA
  return(tidyr::drop_na(tbl, -species_name))
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
#' @param population Population for which to compute linkage disequilibrium. See
#'   \code{\link[ensemblr]{get_populations}} on how to find available
#'   populations for a species.
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
  progress <- dplyr::if_else(progress_bar && interactive(), 'text', 'none')
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
  progress <- dplyr::if_else(progress_bar && interactive(), 'text', 'none')
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
  progress <- dplyr::if_else(progress_bar && interactive(), 'text', 'none')
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
get_ld_variants_by_pair_combn <- function(variant_id,
                                    species_name = 'homo_sapiens',
                                    population = '1000GENOMES:phase_3:CEU',
                                    d_prime = 0.0,
                                    r_squared = 0.0,
                                    verbose = FALSE,
                                    warnings = TRUE,
                                    progress_bar = TRUE) {

  # Assert species_name is scalar
  # (defer more specific assertions to get_ld_variants_by_pair())
  assertthat::assert_that(assertthat::is.scalar(species_name))

  # Assert population is scalar
  # (defer more specific assertions to get_ld_variants_by_pair())
  assertthat::assert_that(assertthat::is.scalar(population))

  # Assert d_prime is scalar
  # (defer more specific assertions to get_ld_variants_by_pair())
  assertthat::assert_that(assertthat::is.scalar(d_prime))

  # Assert r_squared is scalar
  # (defer more specific assertions to get_ld_variants_by_pair())
  assertthat::assert_that(assertthat::is.scalar(r_squared))

  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  pair_combn <- pairwise_combn(variant_id)

  my_ld_variants <- get_ld_variants_by_pair(
    variant_id1 = dplyr::pull(pair_combn, 1L),
    variant_id2 = dplyr::pull(pair_combn, 2L),
    species_name = species_name,
    population = population,
    d_prime = d_prime,
    r_squared = r_squared,
    verbose = verbose,
    warnings = warnings,
    progress_bar = progress_bar
  )

  return(my_ld_variants)
}
