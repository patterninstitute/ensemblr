parse_karyotypes <- function(species_name, lst) {
  tibble::tibble(
    species_name = species_name,
    chromosome = lst$karyotype,
  ) %>%
    dplyr::left_join(lst$top_level_region, by = c(chromosome = "name")) %>%
    dplyr::relocate("species_name", "coord_system", "chromosome", "length")
}

#' Get the karyotype of a species
#'
#' This function retrieves the set of chromosomes of a species.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}}, each row being a chromosome,
#'   of 4 variables:
#' \describe{
#'   \item{`species_name`}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#' \item{`coord_system`}{Coordinate system type.}
#' \item{`chromosome`}{Chromosome name.}
#' \item{`length`}{Genomic length of the chromsome in base pairs.}
#' }
#'
#' @examples
#' # Get the karyotype of Caenorhabditis elegans
#' get_karyotypes("caenorhabditis_elegans")
#'
#' # Get the karyotype of the Giant panda
#' get_karyotypes("ailuropoda_melanoleuca")
#'
#' @md
#' @export
get_karyotypes <- function(species_name = "homo_sapiens",
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
        chromosome = character(),
        length = integer()
      )
    )
  }

  return(purrr::imap_dfr(
    .x = responses_ok,
    .f = ~ parse_karyotypes(
      species_name = species_name[.y],
      lst = .x$content
    )
  ))
}
