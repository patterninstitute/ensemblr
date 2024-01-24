individuals_tbl <- function(species_name = character(),
                            population = character(),
                            description = character(),
                            individual = character(),
                            gender = character()) {
  tbl <- tibble::tibble(
    species_name = species_name,
    population = population,
    description = description,
    individual = individual,
    gender = gender
  )
  return(tbl)
}

json_list_to_individuals_tbl <- function(species_name, json_list) {
  tbl <- individuals_tbl(
    species_name = species_name,
    population = purrr::pluck(json_list, "name", .default = NA_character_),
    description = purrr::pluck(json_list, "description", .default = NA_character_),
    individual = purrr::pluck(json_list, "individuals", 1, "name", .default = NA_character_),
    gender = purrr::pluck(json_list, "individuals", 1, "gender", .default = NA_character_)
  )

  # Drop rows if all columns except species_name are NA
  return(tidyr::drop_na(tbl, -species_name))
}

#' Get individuals for a population
#'
#' This function retrieves individual-level information. The data is returned as
#' a \code{\link[tibble]{tibble}} where each row is an individual of a given
#' species and the columns are metadata about each individual. See below under
#' section Value for details about each column. Use the function
#' `get_populations()` to discover the available populations for a species.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param population Population name. Find the available populations for a given
#'   species with \code{\link[ensemblr]{get_populations}}.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of 5 variables:
#' \describe{
#'   \item{species_name}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{population}{Population.}
#'   \item{description}{Description of the population.}
#'   \item{individual}{Individual identifier.}
#'   \item{gender}{Individual gender.}
#' }
#'
#' @details # Ensembl REST API endpoints
#'
#' `get_individuals()` makes GET requests to
#' [/info/variation/populations/:species:/:population_name](https://rest.ensembl.org/documentation/info/variation_population_name).
#'
#' @md
#' @examples
#' # Get human individuals for populaton "1000GENOMES:phase_3:CEU" (default)
#' get_individuals()
#'
#' # Get Finnish individuals ("1000GENOMES:phase_3:FIN")
#' get_individuals(population = "1000GENOMES:phase_3:FIN")
#'
#' @export
get_individuals <- function(species_name = "homo_sapiens",
                            population = "1000GENOMES:phase_3:CEU",
                            verbose = FALSE,
                            warnings = TRUE,
                            progress_bar = TRUE) {
  # Assert species_name argument.
  assert_species_name(species_name)
  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  error_msg <- glue::glue(
    "All arguments must have consistent lengths, ",
    "only values of length one are recycled:\n",
    "* Length of `species_name`: {length(species_name)}\n",
    "* Length of `population`: {length(population)}\n"
  )
  if (!are_vec_recyclable(
    species_name,
    population
  )) {
    rlang::abort(error_msg)
  }

  recycled_args <- vctrs::vec_recycle_common(
    species_name,
    population
  )

  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c(
    "species_name",
    "population"
  )

  resource_urls <- glue::glue(
    "/info/variation/populations/",
    "{recycled_args$species_name}/",
    "{recycled_args$population}"
  )

  # Usually we'd use purrr::map here but we opted for plyr::llply
  # for a no frills alternative with progress bar support.
  # progress <- dplyr::if_else(progress_bar && interactive(), 'text', 'none')
  # responses <- plyr::llply(
  #   .data = resource_urls,
  #   .fun = request,
  #   verbose = verbose,
  #   warnings = warnings,
  #   .progress = progress)
  responses <-
    request_parallel(
      resource_urls,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ identical(.x$status, "OK"))

  # If none of the responses were successful then return an empty individuals tibble.
  if (rlang::is_empty(responses_ok)) {
    return(individuals_tbl())
  }

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_individuals_tbl(
        species_name = recycled_args$species_name[.y],
        json_list = .x$content
      )
    )
  )
}
