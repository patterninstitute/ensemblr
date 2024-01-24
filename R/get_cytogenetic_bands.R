cytogenetic_bands_tbl <- function(species_name = character(),
                                  assembly_name = character(),
                                  cytogenetic_band = character(),
                                  chromosome = character(),
                                  start = integer(),
                                  end = integer(),
                                  stain = character(),
                                  strand = integer()) {
  tibble::tibble(
    species_name = species_name,
    assembly_name = assembly_name,
    cytogenetic_band = cytogenetic_band,
    chromosome = chromosome,
    start = start,
    end = end,
    stain = stain,
    strand = strand
  )
}

#' @importFrom rlang .data
parse_cytogenetic_bands <- function(species_name, lst) {
  if (rlang::is_empty(lst$top_level_region$bands)) {
    return(cytogenetic_bands_tbl())
  }

  lst$top_level_region$bands %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    tibble::add_column(species_name = species_name, .before = 1L) %>%
    dplyr::rename(
      cytogenetic_band = .data$id,
      chromosome = .data$seq_region_name
    ) %>%
    dplyr::relocate(
      "species_name",
      "assembly_name",
      "cytogenetic_band",
      "chromosome",
      "start",
      "end",
      "stain",
      "strand"
    )
}

#' Get cytogenetic bands by species
#'
#' This function retrieves cytogenetic bands. If no cytogenetic information is
#' available for the queried species then it will be omitted from in the
#' returned value.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}}, each row being a cytogenetic band,
#'   of 8 variables:
#' \describe{
#'   \item{`species_name`}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#' \item{`assembly_name`}{Assembly name.}
#' \item{`cytogenetic_band`}{Name of the cytogenetic_band.}
#' \item{`chromosome`}{Chromosome name.}
#' \item{`start`}{Genomic start position of the cytogenetic band. Starts at 1.}
#' \item{`end`}{Genomic end position of the cytogenetic band. End position is
#' included in the band interval.}
#' \item{`stain`}{\href{https://en.wikipedia.org/wiki/Giemsa_stain}{Giemsa
#'   stain} results: Giemsa negative, \code{'gneg'}; Giemsa positive, of
#'   increasing intensities, \code{'gpos25'}, \code{'gpos50'}, \code{'gpos75'},
#'   and \code{'gpos100'}; centromeric region, \code{'acen'}; heterochromatin,
#'   either pericentric or telomeric, \code{'gvar'}; and short arm of
#'   acrocentric chromosomes are coded as \code{'stalk'}.}
#' \item{`strand`}{Strand.}
#' }
#'
#' @examples
#' # Get toplevel sequences for the human genome (default)
#' get_cytogenetic_bands()
#'
#' # Get toplevel sequences for Mus musculus
#' get_cytogenetic_bands("mus_musculus")
#'
#' @md
#' @export
get_cytogenetic_bands <- function(species_name = "homo_sapiens",
                                  verbose = FALSE,
                                  warnings = TRUE,
                                  progress_bar = TRUE) {
  resource_urls <- glue::glue(
    "/info/assembly/",
    "{species_name}?bands=1"
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
    return(cytogenetic_bands_tbl())
  }

  return(purrr::imap_dfr(
    .x = responses_ok,
    .f = ~ parse_cytogenetic_bands(
      species_name = species_name[.y],
      lst = .x$content
    )
  ))
}
