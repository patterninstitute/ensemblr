xrefs_details_tbl <- function(species_name = character(),
                              gene = character(),
                              ensembl_db = character(),
                              primary_id = character(),
                              display_id = character(),
                              external_db_name = character(),
                              external_db_display_name = character(),
                              version = character(),
                              info_type = character(),
                              info_text = character(),
                              synonyms = list(),
                              description = character()
                              ) {

  tbl <- tibble::tibble(
    species_name = species_name,
    gene = gene,
    ensembl_db = ensembl_db,
    primary_id = primary_id,
    display_id = display_id,
    external_db_name = external_db_name,
    external_db_display_name = external_db_display_name,
    version = version,
    info_type = info_type,
    info_text = info_text,
    synonyms = synonyms,
    description = description
  )
  return(tbl)
}

json_list_to_xrefs_details_tbl <- function(species_name, gene, ensembl_db, json_list) {

  tbl <- xrefs_details_tbl(
    species_name = species_name,
    gene = gene,
    ensembl_db = ensembl_db,
    primary_id = purrr::pluck(json_list, 'primary_id', .default = NA_character_),
    display_id = purrr::pluck(json_list, 'display_id', .default = NA_character_),
    external_db_name = purrr::pluck(json_list, 'dbname', .default = NA_character_),
    external_db_display_name = purrr::pluck(json_list, 'db_display_name', .default = NA_character_),
    version = purrr::pluck(json_list, 'version', .default = NA_character_),
    info_type = purrr::pluck(json_list, 'info_type', .default = NA_character_),
    info_text = purrr::pluck(json_list, 'info_text', .default = NA_character_),
    synonyms = purrr::pluck(json_list, 'synonyms', .default = list()),
    description = purrr::pluck(json_list, 'description', .default = NA_character_)
  )

  # Convert empty strings to NA_character_
  tbl2 <- empty_strings_to_NA(tbl)

  return(tbl2)
}

#' Get cross references by gene symbol or name
#'
#' This function retrieves cross references by symbol or display name of a gene.
#' The data is returned as a \code{\link[tibble]{tibble}} where each row is a
#' cross reference related to the provided symbol or display name of a gene. See
#' below under section Value for details about each column.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param gene Symbol or display name of a gene, e.g., \code{'ACTB'} or
#'   \code{'BRCA2'}.
#' @param ensembl_db Restrict the search to a database other than the default.
#'   Ensembl's default database is \code{'core'}.
#' @param external_db Filter by external database, e.g. \code{'HGNC'}. An empty
#'   string indicates no filtering.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of 12 variables:
#' \describe{
#'   \item{species_name}{Ensembl species name: this is the name used internally
#'   by Ensembl to uniquely identify a species by name. It is the scientific
#'   name but formatted without capitalisation and spacing converted with an
#'   underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{gene}{Gene symbol.}
#'   \item{ensembl_db}{Ensembl database.}
#'   \item{primary_id}{Primary identification in external database.}
#'   \item{display_id}{Display identification in external database.}
#'   \item{external_db_name}{External database name.}
#'   \item{external_db_display_name}{External database display name.}
#'   \item{version}{TODO}
#'   \item{info_type}{There are two types of external cross references (XRef):
#'   direct (\code{'DIRECT'}) or dependent (\code{'DEPENDENT'}). A direct cross
#'   reference is one that can be directly linked to a gene, transcript or
#'   translation object in Ensembl Genomes by synonymy or sequence similarity. A
#'   dependent cross reference is one that is transitively linked to the object
#'   via the direct cross reference. The value can also be \code{'UNMAPPED'} for
#'   unmapped cross references, or \code{'PROJECTION'} for TODO.}
#'   \item{info_text}{TODO}
#'   \item{synonyms}{Other names or acronyms used to refer to the
#'   gene. Note that this column is of the list type.}
#'   \item{description}{Brief description of the external database entry.}
#' }
#'
#' @examples
#' # Get cross references that relate to gene BRCA2
#' get_xrefs_by_gene(species_name = 'human', gene = 'BRCA2')
#'
#' @export
get_xrefs_by_gene <- function(species_name,
                              gene,
                              ensembl_db = 'core',
                              external_db = '',
                              verbose = FALSE,
                              warnings = TRUE,
                              progress_bar = TRUE) {

  # Assert species_name argument.
  assert_species_name(species_name)

  # TODO: Need to validate here the arguments:
  #       - gene
  #       - ensembl_db
  #       - external_db

  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  error_msg <- glue::glue(
    'All arguments must have consistent lengths, ',
    'only values of length one are recycled:\n',
    '* Length of `species_name`: {length(species_name)}\n',
    '* Length of `gene`: {length(gene)}\n',
    '* Length of `ensembl_db`: {length(ensembl_db)}\n',
    '* Length of `external_db`: {length(external_db)}\n'
  )

  if (!are_vec_recyclable(species_name, gene, ensembl_db, external_db)) {
    rlang::abort(error_msg)
  }

  recycled_args <- vctrs::vec_recycle_common(species_name, gene, ensembl_db, external_db)

  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c('species_name', 'gene', 'ensembl_db', 'external_db')

  resource_urls <- glue::glue('/xrefs/name/',
                              '{recycled_args$species_name}/',
                              '{recycled_args$gene}?',
                              'db_type={recycled_args$ensembl_db};',
                              '{p("external_db", recycled_args$external_db)}')

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
  responses_ok <- purrr::keep(responses, ~ identical(.x$status, 'OK'))

  # If none of the responses were successful then return an empty linkage
  # disequilibrium tibble.
  if (rlang::is_empty(responses_ok)) return(ld_tbl())

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_xrefs_details_tbl(
        species_name = recycled_args$species_name[.y],
        gene = recycled_args$gene[.y],
        ensembl_db = recycled_args$ensembl_db[.y],
        json_list = .x$content
      )
    )
  )

}

xrefs_details_tbl2 <- function(species_name = character(),
                              ensembl_id = character(),
                              ensembl_db = character(),
                              primary_id = character(),
                              display_id = character(),
                              external_db_name = character(),
                              external_db_display_name = character(),
                              version = character(),
                              info_type = character(),
                              info_text = character(),
                              synonyms = list(),
                              description = character()
) {

  tbl <- tibble::tibble(
    species_name = species_name,
    ensembl_id = ensembl_id,
    ensembl_db = ensembl_db,
    primary_id = primary_id,
    display_id = display_id,
    external_db_name = external_db_name,
    external_db_display_name = external_db_display_name,
    version = version,
    info_type = info_type,
    info_text = info_text,
    synonyms = synonyms,
    description = description
  )
  return(tbl)
}

json_list_to_xrefs_details_tbl2 <- function(species_name, ensembl_id, ensembl_db, json_list) {

  tbl <- xrefs_details_tbl2(
    species_name = species_name,
    ensembl_id = ensembl_id,
    ensembl_db = ensembl_db,
    primary_id = purrr::pluck(json_list, 'primary_id', .default = NA_character_),
    display_id = purrr::pluck(json_list, 'display_id', .default = NA_character_),
    external_db_name = purrr::pluck(json_list, 'dbname', .default = NA_character_),
    external_db_display_name = purrr::pluck(json_list, 'db_display_name', .default = NA_character_),
    version = purrr::pluck(json_list, 'version', .default = NA_character_),
    info_type = purrr::pluck(json_list, 'info_type', .default = NA_character_),
    info_text = purrr::pluck(json_list, 'info_text', .default = NA_character_),
    synonyms = purrr::pluck(json_list, 'synonyms', .default = list()),
    description = purrr::pluck(json_list, 'description', .default = NA_character_)
  )

  # Convert empty strings to NA_character_
  tbl2 <- empty_strings_to_NA(tbl)

  return(tbl2)
}

# json_list_to_xrefs_details_tbl3 <- function(species_name, ensembl_id, ensembl_db, json_list) {
#
#   tbl <- xrefs_details_tbl2(
#     species_name = species_name,
#     ensembl_id = ensembl_id,
#     ensembl_db = ensembl_db,
#     primary_id = purrr::pluck(json_list, 'primary_id', .default = NA_character_),
#     display_id = purrr::pluck(json_list, 'display_id', .default = NA_character_),
#     external_db_name = purrr::pluck(json_list, 'dbname', .default = NA_character_),
#     external_db_display_name = purrr::pluck(json_list, 'db_display_name', .default = NA_character_),
#     version = purrr::pluck(json_list, 'version', .default = NA_character_),
#     info_type = purrr::pluck(json_list, 'info_type', .default = NA_character_),
#     info_text = purrr::pluck(json_list, 'info_text', .default = NA_character_),
#     synonyms = purrr::pluck(json_list, 'synonyms', .default = list()),
#     description = purrr::pluck(json_list, 'description', .default = NA_character_)
#   )
#
#   # Convert empty strings to NA_character_
#   tbl2 <- empty_strings_to_NA(tbl)
#
#   return(tbl2)
# }
#
# json_list_to_xrefs_details_tbl4 <- function(species_name, ensembl_id, all_levels, ensembl_db, json_list) {
#
#   all_levels <- identical(all_levels, '1')
#
#   if(!all_levels) {
#     return(
#     json_list_to_xrefs_details_tbl2(
#       species_name = species_name,
#       ensembl_id = ensembl_id,
#       ensembl_db = ensembl_db,
#       json_list = json_list
#     ))
#   } else {
#     return(
#       json_list_to_xrefs_details_tbl3(
#         species_name = species_name,
#         ensembl_id = ensembl_id,
#         ensembl_db = ensembl_db,
#         json_list = json_list
#       ))
#   }
# }

#' Get cross-references by Ensembl ID
#'
#' This function retrieves cross-references to external databases by Ensembl
#' identifier. The data is returned as a \code{\link[tibble]{tibble}} where each
#' row is a cross reference related to the provided Ensembl identifier. See
#' below under section Value for details about each column.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Examples: \code{'homo_sapiens'}
#'   (human), \code{'ovis_aries'} (Domestic sheep) or \code{'capra_hircus'}
#'   (Goat).
#' @param ensembl_id An Ensembl stable identifier, e.g. `"ENSG00000248234378"`.
#' @param all_levels A `logical` vector. Set to find all genetic features linked
#'   to the stable ID, and fetch all external references for them. Specifying
#'   this on a gene will also return values from its transcripts and
#'   translations.
#' @param ensembl_db Restrict the search to an Ensembl database: typically one
#'   of `'core'`, `'rnaseq'`, `'cdna'`, `'funcgen'` and `'otherfeatures'`.
#' @param external_db External database to be filtered by. By default no
#'   filtering is applied.
#' @param feature Restrict search to a feature type: gene (\code{'gene'}), exon
#'   (\code{'exon'}), transcript (\code{'transcript'}), and protein
#'   (\code{'translation'}).
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of `r ncol(xrefs_details_tbl2())`
#'   variables:
#' \describe{
#'   \item{`species_name`}{Ensembl species name: this is the name used
#'   internally by Ensembl to uniquely identify a species by name. It is the
#'   scientific name but formatted without capitalisation and spacing converted
#'   with an underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{`ensembl_id`}{An Ensembl stable identifier, e.g. \code{"ENSG00000248234378"}.}
#'   \item{`ensembl_db`}{Ensembl database.}
#'   \item{`primary_id`}{Primary identification in external database.}
#'   \item{`display_id`}{Display identification in external database.}
#'   \item{`external_db_name`}{External database name.}
#'   \item{`external_db_display_name`}{External database display name.}
#'   \item{`version`}{TODO}
#'   \item{`info_type`}{There are two types of external cross references (XRef):
#'   direct (\code{'DIRECT'}) or dependent (\code{'DEPENDENT'}). A direct cross
#'   reference is one that can be directly linked to a gene, transcript or
#'   translation object in Ensembl Genomes by synonymy or sequence similarity. A
#'   dependent cross reference is one that is transitively linked to the object
#'   via the direct cross reference. The value can also be \code{'UNMAPPED'} for
#'   unmapped cross references, or \code{'PROJECTION'} for TODO.}
#'   \item{`info_text`}{TODO}
#'   \item{`synonyms`}{Other names or acronyms used to refer to the
#'   the external database entry. Note that this column is of the list type.}
#'   \item{`description`}{Brief description of the external database entry.}
#' }
#'
#'
#' @md
#' @export
get_xrefs_by_ensembl_id <- function(species_name,
                                    ensembl_id,
                                    all_levels = FALSE,
                                    ensembl_db = 'core',
                                    external_db = '',
                                    feature = '',
                                    verbose = FALSE,
                                    warnings = TRUE,
                                    progress_bar = TRUE) {

  # Assert species_name argument.
  assert_species_name(species_name)

  # TODO: Need to validate here the arguments:
  #       - ensembl_id
  #       - all_levels
  #       - gene
  #       - ensembl_db
  #       - external_db
  #       - feature

  # Assert all_levels argument.
  assertthat::assert_that(rlang::is_logical(all_levels))
  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  all_levels <- ifelse(all_levels, '1', '0')

  error_msg <- glue::glue(
    'All arguments must have consistent lengths, ',
    'only values of length one are recycled:\n',
    '* Length of `species_name`: {length(species_name)}\n',
    '* Length of `ensembl_id`: {length(ensembl_id)}\n',
    '* Length of `all_levels`: {length(all_levels)}\n',
    '* Length of `ensembl_db`: {length(ensembl_db)}\n',
    '* Length of `external_db`: {length(external_db)}\n',
    '* Length of `feature`: {length(feature)}\n'
  )

  if (!are_vec_recyclable(species_name,
                          ensembl_id,
                          all_levels,
                          ensembl_db,
                          external_db,
                          feature)) {
    rlang::abort(error_msg)
  }

  recycled_args <- vctrs::vec_recycle_common(species_name,
                                             ensembl_id,
                                             all_levels,
                                             ensembl_db,
                                             external_db,
                                             feature)

  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <-
    c(
      'species_name',
      'ensembl_id',
      'all_levels',
      'ensembl_db',
      'external_db',
      'feature'
    )

  resource_urls <- glue::glue('/xrefs/id/',
                              '{recycled_args$ensembl_id}?',
                              ';{p("all_levels", recycled_args$all_levels)}',
                              ';{p("db_type", recycled_args$ensembl_db)}',
                              ';{p("external_db", recycled_args$external_db)}',
                              ';{p("object_type", recycled_args$feature)}',
                              ';{p("species", recycled_args$species_name)}')

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
  if (rlang::is_empty(responses_ok)) return(xrefs_details_tbl2())

  #return(tibble::as_tibble(responses_ok[[1]]$content, .name_repair = 'unique'))

  return(purrr::imap_dfr(
    .x = responses_ok,
    .f = ~ json_list_to_xrefs_details_tbl2(
      species_name = recycled_args$species_name[.y],
      ensembl_id = recycled_args$ensembl_id[.y],
      ensembl_db = recycled_args$ensembl_db[.y],
      json_list = .x$content
    )
  ))

}
