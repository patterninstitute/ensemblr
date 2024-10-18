population_tbl <- function(species_name = character(),
                           population = character(),
                           description = character(),
                           cohort_size = integer()) {
  tbl <- tibble::tibble(
    species_name = species_name,
    population = population,
    description = description,
    cohort_size = cohort_size
  )
  return(tbl)
}

json_list_to_population_tbl <- function(species_name, json_list) {
  tbl <- population_tbl(
    species_name = species_name,
    population = purrr::pluck(json_list, 'name', .default = NA_character_),
    description = purrr::pluck(json_list, 'description', .default = NA_character_),
    cohort_size = purrr::pluck(json_list, 'size', .default = NA_integer_)
  )

  # Drop rows if all columns except species_name are NA
  return(tidyr::drop_na(tbl,-species_name))
}

#' Get populations for a species
#'
#' This function retrieves population-level information. The data is returned as
#' a \code{\link[tibble]{tibble}} where each row is a population of a given
#' species and the columns are metadata about each population. See below under
#' section Value for details about each column. The parameter \code{ld_only} to
#' restrict populations returned to only populations with linkage disequilibrium
#' information.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param ld_only Whether to restrict populations returned to only populations
#'   with linkage disequilibrium data.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of 4 variables:
#' \describe{
#'   \item{species_name}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{population}{Population.}
#'   \item{description}{Description of the population.}
#'   \item{cohort_size}{Cohort sample size.}
#' }
#'
#' @details # Ensembl REST API endpoints
#'
#' `get_populations()` makes GET requests to
#' [/info/variation/populations/:species](https://rest.ensembl.org/documentation/info/variation_populations).
#'
#' @examples
#' # Get all human populations with linkage disequilibrium data
#' get_populations(species_name = 'homo_sapiens', ld_only = TRUE)
#'
#' # Get all human populations
#' get_populations(species_name = 'homo_sapiens', ld_only = FALSE)
#'
#' @md
#' @export
get_populations <- function(species_name = 'homo_sapiens',
                            ld_only = TRUE,
                            verbose = FALSE,
                            warnings = TRUE,
                            progress_bar = TRUE) {

  # Assert species_name argument.
  assert_species_name(species_name)
  # Assert ld_only argument.
  assertthat::assert_that(rlang::is_logical(ld_only))
  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  error_msg <- glue::glue(
    'All arguments must have consistent lengths, ',
    'only values of length one are recycled:\n',
    '* Length of `species_name`: {length(species_name)}\n',
    '* Length of `ld_only`: {length(ld_only)}\n'
    )
  if (!are_vec_recyclable(species_name,
                          ld_only))
    rlang::abort(error_msg)

  recycled_args <- vctrs::vec_recycle_common(species_name,
                                             ld_only)

  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c(
    'species_name',
    'ld_only')

  filter_by_ld <- dplyr::if_else(recycled_args$ld_only, '?filter=LD', '')
  resource_urls <- glue::glue(
    '/info/variation/populations/',
    '{recycled_args$species_name}',
    '{filter_by_ld}'
  )

  responses <-
    request_parallel(
      resource_urls,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ .x$status == 'OK')

  # If none of the responses were successful then return an empty population tibble.
  if (rlang::is_empty(responses_ok)) return(population_tbl())

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_population_tbl(
        species_name = recycled_args$species_name[.y],
        json_list = .x$content
      )
    )
  )
}
