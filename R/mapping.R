#' Convert strand values to integer codes
#'
#' This function converts a character vector of values \code{'forward'} or
#' \code{'reverse'} to \code{1} or \code{-1}, respectively.
#'
#' @param strand Character vector of strandness values: \code{'forward'} or
#'   \code{'reverse'}.
#' @param .default What value to use by default for values other than
#'   \code{'forward'} or \code{'reverse'}.
#' @param .missing What value to use when recoding \code{NA_character_}.
#'
#' @return An integer vector.
#' @seealso \code{\link{code_to_strand}}
#' @keywords internal
strand_to_code <- function(strand, .default = NA_integer_, .missing = NA_integer_) {
  # Is .default an integer scalar?
  assertthat::assert_that(
    rlang::is_scalar_integer(.default),
    msg = "`.default` must be an integer scalar."
  )

  # Is .missing an integer scalar?
  assertthat::assert_that(
    rlang::is_scalar_integer(.missing),
    msg = "`.missing` must be an integer scalar."
  )

  dplyr::recode(strand,
    `forward` = 1L,
    `reverse` = -1L,
    .default = .default,
    .missing = .missing
  )
}

#' Convert strand integer codes to strand words
#'
#' This function converts an integer vector of values \code{1L} or \code{-1L} to
#' \code{'forward'} or \code{'reverse'}, respectively.
#'
#' @param code Integer vector of strandness codes: \code{1L} or
#'   \code{-1L}.
#' @param .default What value to use by default for values other than
#'   \code{1L} or \code{-1L}.
#' @param .missing What value to use when recoding \code{NA_integer_}.
#'
#' @return A character vector.
#'
#' @seealso \code{\link{strand_to_code}}
#' @keywords internal
code_to_strand <- function(code, .default = NA_character_, .missing = NA_character_) {
  # Is strand an integer scalar?
  assertthat::assert_that(
    rlang::is_integer(code),
    msg = "`code` must be an integer vector."
  )

  # Is .default a character scalar?
  assertthat::assert_that(
    rlang::is_scalar_character(.default),
    msg = "`.default` must be an character scalar."
  )

  # Is .missing an integer scalar?
  assertthat::assert_that(
    rlang::is_scalar_character(.missing),
    msg = "`.missing` must be an character scalar."
  )

  dplyr::recode(code,
    `1` = "forward",
    `-1` = "reverse",
    .default = .default,
    .missing = .missing
  )
}

mapping_tbl <- function(
    species_name = character(),
    assembly_0 = character(),
    assembly_1 = character(),
    assembly_2 = character(),
    coordinate_system_0 = character(),
    coordinate_system_1 = character(),
    coordinate_system_2 = character(),
    strand_0 = character(),
    strand_1 = character(),
    strand_2 = character(),
    sequence_region_name_0 = character(),
    sequence_region_name_1 = character(),
    sequence_region_name_2 = character(),
    start_0 = integer(),
    start_1 = integer(),
    start_2 = integer(),
    end_0 = integer(),
    end_1 = integer(),
    end_2 = integer()) {
  tbl <- tibble::tibble(
    species_name = species_name,
    assembly_0 = assembly_0,
    assembly_1 = assembly_1,
    assembly_2 = assembly_2,
    coordinate_system_0 = coordinate_system_0,
    coordinate_system_1 = coordinate_system_1,
    coordinate_system_2 = coordinate_system_2,
    strand_0 = strand_0,
    strand_1 = strand_1,
    strand_2 = strand_2,
    sequence_region_name_0 = sequence_region_name_0,
    sequence_region_name_1 = sequence_region_name_1,
    sequence_region_name_2 = sequence_region_name_2,
    start_0 = start_0,
    start_1 = start_1,
    start_2 = start_2,
    end_0 = end_0,
    end_1 = end_1,
    end_2 = end_2
  )

  return(tbl)
}

json_list_to_mapping_tbl <- function(species_name,
                                     assembly_0,
                                     strand_0,
                                     coordinate_system_0,
                                     sequence_region_name_0,
                                     start_0,
                                     end_0,
                                     json_list) {
  tbl <- mapping_tbl(
    species_name = species_name,
    assembly_0 = assembly_0,
    assembly_1 = purrr::pluck(json_list, "original", "assembly", .default = NA_character_),
    assembly_2 = purrr::pluck(json_list, "mapped", "assembly", .default = NA_character_),
    coordinate_system_0 = coordinate_system_0,
    coordinate_system_1 = purrr::pluck(json_list, "original", "coord_system", .default = NA_character_),
    coordinate_system_2 = purrr::pluck(json_list, "mapped", "coord_system", .default = NA_character_),
    strand_0 = strand_0,
    strand_1 = code_to_strand(
      as.integer(purrr::pluck(json_list, "original", "strand", .default = NA_integer_))
    ),
    strand_2 = code_to_strand(
      as.integer(purrr::pluck(json_list, "mapped", "strand", .default = NA_integer_))
    ),
    sequence_region_name_0 = sequence_region_name_0,
    sequence_region_name_1 = purrr::pluck(json_list, "original", "seq_region_name", .default = NA_character_),
    sequence_region_name_2 = purrr::pluck(json_list, "mapped", "seq_region_name", .default = NA_character_),
    start_0 = start_0,
    start_1 = purrr::pluck(json_list, "original", "start", .default = NA_integer_),
    start_2 = purrr::pluck(json_list, "mapped", "start", .default = NA_integer_),
    end_0 = end_0,
    end_1 = purrr::pluck(json_list, "original", "end", .default = NA_integer_),
    end_2 = purrr::pluck(json_list, "mapped", "end", .default = NA_integer_)
  )
  return(tbl)
}

remap_gdna_to_gdna <- function(genomic_range,
                               from = "GRCh37",
                               to = "GRCh38",
                               strand = "forward",
                               species_name = "homo_sapiens",
                               coord_system_from = "chromosome",
                               coord_system_to = "chromosome",
                               verbose = FALSE,
                               warnings = TRUE,
                               progress_bar = TRUE) {
  # Is `from` a character vector of non-NA values?
  assertthat::assert_that(
    rlang::is_character(from),
    !any(rlang::are_na(from)),
    msg = "Argument `from` must be a character vector of non-NA values."
  )

  # Is `to` a character vector of non-NA values?
  assertthat::assert_that(
    rlang::is_character(to),
    !any(rlang::are_na(to)),
    msg = "Argument `to` must be a character vector of non-NA values."
  )

  # Is `strand` a character vector of 'forward' or 'reverse' values only?
  assertthat::assert_that(
    rlang::is_character(strand),
    !any(rlang::are_na(strand)),
    all(strand %in% c("forward", "reverse")),
    msg = "Argument `to` must be a character vector of non-NA values."
  )

  # Is `coord_system_from` a scalar character vector 'chromosome' values only?
  assertthat::assert_that(
    rlang::is_character(coord_system_from),
    !any(rlang::are_na(coord_system_from)),
    all(coord_system_from %in% c("chromosome")),
    msg = "For the moment argument `coord_system_from` must be a character vector of 'chromosome' values only."
  )

  # Is `coord_system_to` a scalar character vector 'chromosome' values only?
  assertthat::assert_that(
    rlang::is_character(coord_system_to),
    !any(rlang::are_na(coord_system_to)),
    all(coord_system_to %in% c("chromosome")),
    msg = "For the moment argument `coord_system_to` must be a character vector of 'chromosome' values only."
  )

  # Assert genomic_range argument.
  assert_genomic_range(genomic_range)
  # Assert species_name argument.
  assert_species_name(species_name)

  # Assert verbose argument.
  assertthat::assert_that(assertthat::is.flag(verbose))
  # Assert warnings argument.
  assertthat::assert_that(assertthat::is.flag(warnings))
  # Assert progress_bar argument.
  assertthat::assert_that(assertthat::is.flag(progress_bar))

  error_msg <- glue::glue(
    "All arguments must have consistent lengths, ",
    "only values of length one are recycled:\n",
    "* Length of `genomic_range`: {length(genomic_range)}\n",
    "* Length of `species_name`: {length(species_name)}\n",
    "* Length of `from`: {length(from)}\n",
    "* Length of `to`: {length(to)}\n",
    "* Length of `strand`: {length(strand)}\n",
    "* Length of `coord_system_from`: {length(coord_system_from)}\n",
    "* Length of `coord_system_to`: {length(coord_system_to)}\n"
  )
  if (!are_vec_recyclable(
    genomic_range,
    species_name,
    from,
    to,
    strand,
    coord_system_from,
    coord_system_to
  )) {
    rlang::abort(error_msg)
  }

  recycled_args <- vctrs::vec_recycle_common(
    genomic_range,
    species_name,
    from,
    to,
    strand,
    coord_system_from,
    coord_system_to
  )
  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c(
    "genomic_range",
    "species_name",
    "from",
    "to",
    "strand",
    "coord_system_from",
    "coord_system_to"
  )

  resource_urls <- glue::glue(
    "/map/",
    "{recycled_args$species_name}/",
    "{recycled_args$from}/",
    "{recycled_args$genomic_range}:{strand_to_code(recycled_args$strand)}/",
    "{recycled_args$to}/?",
    "coord_system={recycled_args$coord_system_from};",
    "target_coord_system={recycled_args$coord_system_to}"
  )

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
    return(mapping_tbl())
  }

  return(
    purrr::imap_dfr(
      .x = responses_ok,
      .f = ~ json_list_to_mapping_tbl(
        species_name = recycled_args$species_name[.y],
        assembly_0 = recycled_args$from[.y],
        strand_0 = recycled_args$strand[.y],
        coordinate_system_0 = recycled_args$coord_system_from[.y],
        sequence_region_name_0 = split_genomic_range(recycled_args$genomic_range[.y])[, "chromosome"],
        start_0 = split_genomic_range(recycled_args$genomic_range[.y])[, "start"],
        end_0 = split_genomic_range(recycled_args$genomic_range[.y])[, "end"],
        json_list = .x$content$mappings
      )
    )
  )
}
