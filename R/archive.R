id_tbl <- function(
  id = character(),
  id_latest = character(),
  type = character(),
  id_version = integer(),
  release = integer(),
  is_current = logical(),
  genome_assembly_name = character(),
  peptide = character(),
  possible_replacement = list()
) {
  tbl <- tibble::tibble(
    id = id,
    id_latest = id_latest,
    type = type,
    id_version = id_version,
    release = release,
    is_current = is_current,
    genome_assembly_name = genome_assembly_name,
    peptide = peptide,
    possible_replacement = possible_replacement
  )

  return(tbl)
}

json_list_to_id_tbl <- function(json_list) {

  tbl <- id_tbl(
    id = purrr::pluck(json_list, 'id', .default = NA_character_),
    id_latest = purrr::pluck(json_list, 'latest', .default = NA_character_),
    type = purrr::pluck(json_list, 'type', .default = NA_character_),
    id_version = as.integer(purrr::pluck(json_list, 'version', .default = NA_integer_)),
    release = as.integer(purrr::pluck(json_list, 'release', .default = NA_integer_)),
    is_current = purrr::pluck(json_list, 'is_current', .default = NA) == '1',
    genome_assembly_name = purrr::pluck(json_list, 'assembly', .default = NA_character_),
    peptide = purrr::pluck(json_list, 'peptide', .default = NA_character_),
    possible_replacement = purrr::pluck(json_list, 'possible_replacement', .default = list(character()))
  )

  return(tbl)
}

#' Get details about an Ensembl identifier
#'
#' This function retrieves information about one or more Ensembl identifiers.
#' Ensembl identifiers for which information is available are: genes, exons,
#' transcripts and proteins.
#'
#' @param id A character vector of Ensembl identifiers. Ensembl identifiers have
#'   the form ENS[species prefix][feature type prefix][a unique eleven digit
#'   number]. \code{id} should not contain NAs. Please note that while
#'   \code{'ENSG00000157764'} is a valid identifier as a query,
#'   \code{'ENSG00000157764.13'} is not.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of 9 variables:
#' \describe{
#'   \item{\code{id}}{Ensembl identifier.}
#'   \item{\code{id_latest}}{Ensembl identifier including the version suffix.}
#'   \item{\code{type}}{Entity type: gene (\code{'Gene'}), exon (\code{'Exon'}),
#'   transcript (\code{'Transcript'}), and protein (\code{'Translation'}).}
#'   \item{\code{id_version}}{Ensembl identifier version, indicates how many
#'   times that entity has changed during its time in Ensembl.}
#'   \item{\code{release}}{Ensembl release version.}
#'   \item{\code{is_current}}{Is this the latest identifier for the represented entity.}
#'   \item{\code{genome_assembly_name}}{Code name of the genome assembly.}
#'   \item{\code{peptide}}{TODO}
#'   \item{\code{possible_replacement}}{TODO}
#' }
#'
#' @examples
#' get_id(c('ENSDARE00000830915', 'ENSG00000248378', 'ENSDART00000033574', 'ENSP00000000233'))
#'
#' @export
get_id <- function(id,
                   verbose = FALSE,
                   warnings = TRUE,
                   progress_bar = TRUE) {


  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  # e() is a short alias for function urltools::url_encode()
  e <- urltools::url_encode

  resource_urls <- glue::glue(
    '/archive/id/',
    '{e(id)}?'
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
  responses_ok <- purrr::keep(responses, ~ identical(.x$status, 'OK'))

  # If none of the responses were successful then return an empty linkage
  # disequilibrium tibble.
  if (rlang::is_empty(responses_ok)) return(id_tbl())

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_id_tbl(json_list = .x$content)
    )
  )
}
