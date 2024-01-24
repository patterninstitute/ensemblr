#' @importFrom rlang .data
parse_toplevel_sequences <- function(species_name, lst) {
  lst$top_level_region %>%
    tibble::as_tibble() %>%
    tibble::add_column(species_name = species_name) %>%
    dplyr::rename(toplevel_sequence = .data$name) %>%
    dplyr::relocate("species_name", "coord_system", "toplevel_sequence", "length")
}

#' Get toplevel sequences by species
#'
#' This function retrieves toplevel sequences. These sequences correspond to
#' genomic regions in the genome assembly that are not a component of another
#' sequence region. Thus, toplevel sequences will be chromosomes and any
#' unlocalised or unplaced scaffolds.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}}, each row being a toplevel sequence,
#'   of 4 variables:
#' \describe{
#'   \item{`species_name`}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#' \item{`coord_system`}{Coordinate system type.}
#' \item{`toplevel_sequence`}{Name of the toplevel sequence.}
#' \item{`length`}{Genomic length toplevel sequence in base pairs.}
#' }
#'
#' @examples
#' # Get toplevel sequences for the human genome (default)
#' get_toplevel_sequences()
#'
#' # Get toplevel sequences for Caenorhabditis elegans
#' get_toplevel_sequences("caenorhabditis_elegans")
#'
#' @md
#' @export
get_toplevel_sequences <- function(species_name = "homo_sapiens",
                                   verbose = FALSE,
                                   warnings = TRUE,
                                   progress_bar = TRUE) {
  resource_urls <- glue::glue(
    "/info/assembly/",
    "{species_name}?bands=0"
  )

  responses <-
    request_parallel(
      resource_urls,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <-
    purrr::keep(
      responses,
      ~ identical(.x$status, "OK") &&
        !rlang::is_empty(.x$content)
    )
  if (rlang::is_empty(responses_ok)) {
    return(
      tibble::tibble(
        species_name = character(),
        coord_system = character(),
        toplevel_sequence = character(),
        length = integer()
      )
    )
  }

  return(purrr::imap_dfr(
    .x = responses_ok,
    .f = ~ parse_toplevel_sequences(
      species_name = species_name[.y],
      lst = .x$content
    )
  ))
}
