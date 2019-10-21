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
  responses_ok <- purrr::keep(responses, ~ .x$status == 'OK')

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

