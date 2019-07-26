species_tbl <- function(
  division = character(),
  taxon_id = integer(),
  species_name = character(),
  species_display_name = character(),
  species_common_name = character(),
  release = integer(),
  genome_assembly_name = character(),
  genbank_assembly_accession = character(),
  strain = character(),
  strain_collection = character(),
  species_aliases = list(),
  groups = list()
) {
  tbl <- tibble::tibble(
    division = division,
    taxon_id = taxon_id,
    species_name = species_name,
    species_display_name = species_display_name,
    species_common_name = species_common_name,
    release = release,
    genome_assembly_name = genome_assembly_name,
    genbank_assembly_accession = genbank_assembly_accession,
    strain = strain,
    strain_collection = strain_collection,
    species_aliases = species_aliases,
    groups = groups
  )
  return(tbl)
}

json_list_to_species_tbl <- function(json_list) {

  tbl <- species_tbl(
    division = purrr::pluck(json_list, 'division', .default = NA_character_),
    taxon_id = as.integer(purrr::pluck(json_list, 'taxon_id', .default = NA_integer_)),
    species_name = purrr::pluck(json_list, 'name', .default = NA_character_),
    species_display_name = purrr::pluck(json_list, 'display_name', .default = NA_character_),
    species_common_name = purrr::pluck(json_list, 'common_name', .default = NA_character_),
    release = as.integer(purrr::pluck(json_list, 'release', .default = NA_integer_)),
    genome_assembly_name = purrr::pluck(json_list, 'assembly', .default = NA_character_),
    genbank_assembly_accession = purrr::pluck(json_list, 'accession', .default = NA_character_),
    strain = purrr::pluck(json_list, 'strain', .default = NA_character_),
    strain_collection = purrr::pluck(json_list, 'strain_collection', .default = NA_character_),
    species_aliases = purrr::pluck(json_list, 'aliases', .default = list()),
    groups = purrr::pluck(json_list, 'groups', .default = list())
  )

  # Sort species by division and species_name
  division <- rlang::expr(division)
  species_name <- rlang::expr(species_name)

  return(dplyr::arrange(tbl, !!division, !!species_name))
}

#' Get Ensembl species
#'
#' This function retrieves species-level information. The data is returned as a
#' \code{\link[tibble]{tibble}} where each row is a species and the columns are
#' metadata about each species. See below under section Value for details about
#' each column.
#'
#' @param division Ensembl division, e.g., \code{"EnsemblVertebrates"} or
#'   \code{"EnsemblBacteria"}, or a combination of several divisions. Check
#'   function \code{\link[ensemblr]{get_divisions}} to get available Ensembl
#'   divisions.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of 12 variables:
#' \describe{
#'   \item{division}{Ensembl division: \code{"EnsemblVertebrates"},
#'   \code{"EnsemblMetazoa"}, \code{"EnsemblPlants"}, \code{"EnsemblProtists"},
#'   \code{"EnsemblFungi"} or \code{"EnsemblBacteria"}.}
#'   \item{taxon_id}{NCBI taxon identifier.}
#'   \item{species_name}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{species_display_name}{Species display name: the name used for display
#'   on Ensembl website.}
#'   \item{species_common_name}{Species common name.}
#'   \item{release}{Ensembl release version.}
#'   \item{genome_assembly_name}{Code name of the genome assembly.}
#'   \item{genbank_assembly_accession}{Genbank assembly accession identifier.}
#'   \item{strain}{Species strain.}
#'   \item{strain_collection}{Species strain collection.}
#'   \item{species_aliases}{Other names or acronyms used to refer to the
#'   species. Note that this column is of the list type.}
#'   \item{groups}{Ensembl databases for which data exists for this species.
#'   Note that this column is of the list type.}
#' }
#'
#' @export
get_species <- function(division = get_divisions(),
                        verbose = FALSE,
                        warnings = TRUE,
                        progress_bar = TRUE) {

  # Assert division argument.
  assert_division(division)
  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))

  # e() is a short alias for function urltools::url_encode()
  e <- urltools::url_encode
  resource_urls <- glue::glue(
    '/info/species?',
    'hide_strain_info=0;', # We do not hide strain information
    'division={e(division)}'
  )

  # Usually we'd use purrr::map here but we opted for plyr::llply
  # for a no frills alternative with progress bar support.
  progress <- dplyr::if_else(progress_bar && interactive(), 'text', 'none')
  responses <- plyr::llply(
    .data = resource_urls,
    .fun = request,
    verbose = verbose,
    warnings = warnings,
    .progress = progress)

  # Only keep those responses that responded successfully, i.e. with status == "OK".
  responses_ok <- purrr::keep(responses, ~ .x$status == 'OK')

  # If none of the responses were successful then return an empty linkage
  # disequilibrium tibble.
  if (rlang::is_empty(responses_ok)) return(species_tbl())

  return(
    purrr::map_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_species_tbl(json_list = .x$content$species)
    )
  )
}

