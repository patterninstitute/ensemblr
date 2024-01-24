analysis_tbl <- function(
    species_name = character(),
    database = character(),
    analysis = character()) {
  tbl <- tibble::tibble(
    species_name = species_name,
    database = database,
    analysis = analysis
  )
  return(tbl)
}

json_list_to_analysis_tbl <- function(species_name, json_list) {
  tbl <- purrr::imap_dfr(
    json_list,
    .f = ~ analysis_tbl(
      species_name = species_name,
      database = .x,
      analysis = .y
    )
  )

  # Drop rows if all columns except species_name are NA
  return(tidyr::drop_na(tbl, -species_name))
}

#' Get analyses behind Ensembl databases
#'
#' This function retrieves a table of analyses involved in the generation of
#' data for the different Ensembl databases.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'} (Goat).
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of 3 variables:
#' \describe{
#'   \item{\code{species_name}}{Ensembl species name: this is the name used
#'   internally by Ensembl to uniquely identify a species by name. It is the
#'   scientific name but formatted without capitalisation and spacing converted
#'   with an underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{\code{database}}{Ensembl database. Typically one of \code{'core'},
#'   \code{'rnaseq'}, \code{'cdna'}, \code{'funcgen'} and
#'   \code{'otherfeatures'}.}
#'   \item{\code{analysis}}{Analysis.}
#' }
#'
#' @export
get_analyses <- function(species_name,
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

  # e() is a short alias for function urltools::url_encode()
  e <- urltools::url_encode

  resource_urls <- glue::glue(
    "/info/analysis/",
    "{e(species_name)}?"
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

  # If none of the responses were successful then return an empty linkage
  # disequilibrium tibble.
  if (rlang::is_empty(responses_ok)) {
    return(analysis_tbl())
  }

  tbl <- purrr::imap_dfr(
    .x = responses_ok,
    .f = ~ json_list_to_analysis_tbl(species_name = species_name[.y], json_list = .x$content)
  )

  database <- rlang::expr(database)
  analysis <- rlang::expr(analysis)

  return(dplyr::arrange(tbl, species_name, !!database, !!analysis))
}
