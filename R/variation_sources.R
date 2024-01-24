variation_source_tbl <- function(species_name = character(),
                                 db_name = character(),
                                 type = character(),
                                 version = character(),
                                 somatic_status = character(),
                                 description = character(),
                                 url = character(),
                                 data_types = list()) {
  tbl <- tibble::tibble(
    species_name = species_name,
    db_name = db_name,
    type = type,
    version = version,
    somatic_status = somatic_status,
    description = description,
    url = url,
    data_types = data_types
  )
  return(tbl)
}

json_list_variation_source_tbl <- function(species_name, json_list) {
  tbl <- variation_source_tbl(
    species_name = species_name,
    db_name = purrr::pluck(json_list, "name", .default = NA_character_),
    type = purrr::pluck(json_list, "type", .default = NA_character_),
    version = purrr::pluck(json_list, "version", .default = NA_character_),
    somatic_status = purrr::pluck(json_list, "somatic_status", .default = NA_character_),
    description = purrr::pluck(json_list, "description", .default = NA_character_),
    url = purrr::pluck(json_list, "url", .default = NA_character_),
    data_types = purrr::pluck(json_list, "data_types", .default = list(NA_character_))
  )

  # Convert empty strings to NA_character_
  tbl2 <- empty_strings_to_NA(tbl)

  return(tbl2)
}


#' Retrieve variant sources
#'
#' This function retrieves variant sources, i.e. a list of databases used by
#' Ensembl from which variant information is retrieved.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param progress_bar Whether to show a progress bar.
#' @return A \code{\link[tibble]{tibble}}, each row being a variant database,
#'   of 8 variables:
#' \describe{
#'   \item{species_name}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#' \item{db_name}{Database name.}
#' \item{type}{Database type, e.g., `chip` (genotyping chip) or `lsdb`
#' (locus-specific database).}
#' \item{version}{Database version.}
#' \item{somatic_status}{Somatic status.}
#' \item{description}{Database description.}
#' \item{url}{Database's URL.}
#' \item{data_types}{Data types to be found at database.}
#' }
#'
#' @details # Ensembl REST API endpoints
#'
#' `get_variation_sources` makes GET requests to
#' [info/variation/:species](https://rest.ensembl.org/documentation/info/variation).
#'
#' @md
#' @examples
#' # Retrieve variant sources for human (default)
#' get_variation_sources()
#'
#' # Retrieve variant sources for mouse
#' get_variation_sources(species_name = "mus_musculus")
#'
#' @export
get_variation_sources <- function(species_name = "human",
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

  resource_urls <- glue::glue(
    "/info/variation/",
    "{species_name}/"
  )

  responses <-
    request_parallel(
      resource_urls,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ identical(.x$status, "OK"))

  responses_ok_lgl <- purrr::map_lgl(responses, ~ identical(.x$status, "OK"))
  species_name2 <- species_name[responses_ok_lgl]

  # If none of the responses were successful then return an empty tibble.
  if (rlang::is_empty(responses_ok)) {
    return(variation_source_tbl())
  }

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_variation_source_tbl(
        species_name = species_name2[.y],
        json_list = .x$content
      )
    )
  )
}
