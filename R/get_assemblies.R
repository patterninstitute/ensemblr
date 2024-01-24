assembly_details <- function(species_name = character(),
                             assembly_name = character(),
                             assembly_date = character(),
                             genebuild_method = character(),
                             golden_path_length = integer(),
                             genebuild_initial_release_date = character(),
                             default_coord_system_version = character(),
                             assembly_accession = character(),
                             genebuild_start_date = character(),
                             genebuild_last_geneset_update = character()) {
  tibble::tibble(
    species_name = species_name,
    assembly_name = assembly_name,
    assembly_date = assembly_date,
    genebuild_method = genebuild_method,
    golden_path_length = golden_path_length,
    genebuild_initial_release_date = genebuild_initial_release_date,
    default_coord_system_version = default_coord_system_version,
    assembly_accession = assembly_accession,
    genebuild_start_date = genebuild_start_date,
    genebuild_last_geneset_update = genebuild_last_geneset_update
  )
}

parse_assembly_details <- function(species_name, lst) {
  assembly_details(
    species_name = species_name,
    assembly_name = lst$assembly_name,
    assembly_date = lst$assembly_date,
    genebuild_method = lst$genebuild_method,
    golden_path_length = lst$golden_path,
    genebuild_initial_release_date = lst$genebuild_initial_release_date,
    default_coord_system_version = lst$default_coord_system_version,
    assembly_accession = lst$assembly_accession,
    genebuild_start_date = lst$genebuild_start_date,
    genebuild_last_geneset_update = lst$genebuild_last_geneset_update
  )
}

#' Get details about the genome assembly of a species
#'
#' This functions retrieves details about the assembly of a queried species.
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
#' \item{`assembly_name`}{Assembly name.}
#' \item{`assembly_date`}{Assembly date.}
#' \item{`genebuild_method`}{Annotation method.}
#' \item{`golden_path_length`}{Golden path length.}
#' \item{`genebuild_initial_release_date`}{Genebuild release date.}
#' \item{`default_coord_system_version`}{Default coordinate system version.}
#' \item{`assembly_accession`}{Assembly accession.}
#' \item{`genebuild_start_date`}{Genebuild start date.}
#' \item{`genebuild_last_geneset_update`}{Genebuild last geneset update.}
#' }
#'
#' @examples
#' # Get details about the human assembly
#' get_assemblies()
#'
#' # Get details about the Mouse and the Fruit Fly genomes
#' get_assemblies(c("mus_musculus", "drosophila_melanogaster"))
#'
#' @md
#' @export
get_assemblies <- function(species_name = "homo_sapiens",
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
  responses_ok <- purrr::keep(responses, ~ identical(.x$status, "OK") && !rlang::is_empty(.x$content))
  if (rlang::is_empty(responses_ok)) {
    return(assembly_details())
  }

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ parse_assembly_details(
        species_name = species_name[.y],
        lst = .x$content
      )
    )
  )
}
