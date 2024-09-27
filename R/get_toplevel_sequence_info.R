toplevel_sequence_info_tbl <- function(species_name = character(),
                                       toplevel_sequence = character(),
                                       is_chromosome = logical(),
                                       coordinate_system = character(),
                                       assembly_exception_type = character(),
                                       is_circular = logical(),
                                       assembly_name = character(),
                                       length = double()) {
  tibble::tibble(species_name = species_name,
                 toplevel_sequence = toplevel_sequence,
                 is_chromosome = is_chromosome,
                 coordinate_system = coordinate_system,
                 assembly_exception_type = assembly_exception_type,
                 is_circular = is_circular,
                 assembly_name = assembly_name,
                 length = length)
}

parse_toplevel_sequence_info <- function(species_name, toplevel_sequence, lst) {

  toplevel_sequence_info_tbl(species_name = species_name,
                             toplevel_sequence = toplevel_sequence,
                             is_chromosome = as.logical(lst$is_chromosome),
                             coordinate_system = lst$coordinate_system,
                             assembly_exception_type = lst$assembly_exception_type,
                             is_circular = as.logical(lst$is_circular),
                             assembly_name = lst$assembly_name,
                             length = lst$length)
}

#' Get toplevel sequences details
#'
#' This function retrieves a few extra details about a toplevel sequence. These
#' sequences correspond to genomic regions in the genome assembly that are not a
#' component of another sequence region. Thus, toplevel sequences will be
#' chromosomes and any unlocalised or unplaced scaffolds.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param toplevel_sequence A toplevel sequence name, e.g. chromosome names such
#'   as `"1"`, `"X"`, or `"Y"`, or a non-chromosome sequence, e.g., a scaffold
#'   such as `"KI270757.1"`.
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}}, each row being a toplevel sequence,
#'   of 8 variables:
#' \describe{
#' \item{`species_name`}{Ensembl species name: this is the name used internally
#' by Ensembl to uniquely identify a species by name. It is the scientific name
#' but formatted without capitalisation and spacing converted with an
#' underscore, e.g., \code{'homo_sapiens'}.}
#' \item{`toplevel_sequence`}{Name of the toplevel sequence.}
#' \item{`is_chromosome`}{A logical indicating whether the toplevel sequence is
#' a chromosome (`TRUE`) or not (`FALSE`).}
#' \item{`coord_system`}{Coordinate system type.}
#' \item{`assembly_exception_type`}{Coordinate system type.}
#' \item{`is_circular`}{A logical indicating whether the toplevel sequence is a
#' circular sequence (`TRUE`) or not (`FALSE`).}
#' \item{`assembly_name`}{Assembly name.}
#' \item{`length`}{Genomic length toplevel sequence in base pairs.}
#' }
#'
#'
#' @md
#' @examples
#' # Get details about human chromosomes (default)
#' get_toplevel_sequence_info()
#'
#' # Get details about a scaffold
#' # (To find available toplevel sequences to query use the function
#' # `get_toplevel_sequences()`)
#' get_toplevel_sequence_info(species_name = 'homo_sapiens', toplevel_sequence = 'KI270757.1')
#'
#' @seealso [get_toplevel_sequences()]
#'
#' @export
get_toplevel_sequence_info <- function(species_name = 'homo_sapiens',
                                       toplevel_sequence = c(1:22, 'X', 'Y', 'MT'),
                                       verbose = FALSE,
                                       warnings = TRUE,
                                       progress_bar = TRUE) {

  error_msg <- glue::glue(
    'All arguments must have consistent lengths, ',
    'only values of length one are recycled:\n',
    '* Length of `species_name`: {length(species_name)}\n',
    '* Length of `toplevel_sequence`: {length(toplevel_sequence)}\n'
  )

  if (!are_vec_recyclable(species_name,
                          toplevel_sequence)) {
    rlang::abort(error_msg)
  }

  recycled_args <- vctrs::vec_recycle_common(species_name,
                                             toplevel_sequence)

  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <-
    c(
      'species_name',
      'toplevel_sequence'
    )

  resource_urls <- glue::glue('/info/assembly/',
                              '{recycled_args$species_name}/{recycled_args$toplevel_sequence}')

  responses <-
    request_parallel(
      resource_urls,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <-
    purrr::keep(responses,
                ~ identical(.x$status, 'OK') &&
                  !rlang::is_empty(.x$content))
  if (rlang::is_empty(responses_ok))
    return(toplevel_sequence_info_tbl())

  return(purrr::imap_dfr(
    .x = responses_ok,
    .f = ~ parse_toplevel_sequence_info(
      species_name = recycled_args$species_name[.y],
      toplevel_sequence = recycled_args$toplevel_sequence[.y],
      lst = .x$content
    )
  ))

}
