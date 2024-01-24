#' @keywords internal
lookup_id_ <- function(ensembl_id,
                       species_name = "homo_sapiens",
                       ensembl_db = "",
                       expand = FALSE,
                       format = "condensed",
                       utr = FALSE,
                       phenotypes = FALSE,
                       verbose = FALSE,
                       warnings = TRUE,
                       progress_bar = TRUE) {
  expand <- dplyr::if_else(expand, "1", "0")
  utr <- dplyr::if_else(utr, "1", "0")
  phenotypes <- dplyr::if_else(phenotypes, "1", "0")

  error_msg <- glue::glue(
    "All arguments must have consistent lengths, ",
    "only values of length one are recycled:\n",
    "* Length of `ensembl_id`: {length(ensembl_id)}\n",
    "* Length of `species_name`: {length(species_name)}\n",
    "* Length of `ensembl_db`: {length(ensembl_db)}\n",
    "* Length of `expand`: {length(expand)}\n",
    "* Length of `format`: {length(format)}\n",
    "* Length of `utr`: {length(utr)}\n",
    "* Length of `phenotypes`: {length(phenotypes)}"
  )
  if (!are_vec_recyclable(
    ensembl_id,
    species_name,
    ensembl_db,
    expand,
    format,
    utr,
    phenotypes
  )) {
    rlang::abort(error_msg)
  }

  recycled_args <- vctrs::vec_recycle_common(
    ensembl_id,
    species_name,
    ensembl_db,
    expand,
    format,
    utr,
    phenotypes
  )
  # The order of names here should be same as passed to
  # vctrs::vec_recycle_common()
  names(recycled_args) <- c(
    "ensembl_id",
    "species_name",
    "ensembl_db",
    "expand",
    "format",
    "utr",
    "phenotypes"
  )

  resource_urls <- glue::glue(
    "/lookup/id/",
    "{recycled_args$ensembl_id}?",
    '{p("species", recycled_args$species_name)};',
    '{p("db_type", recycled_args$ensembl_db)};',
    '{p("expand", recycled_args$expand)};',
    '{p("format", recycled_args$format)};',
    '{p("utr", recycled_args$utr)};',
    '{p("phenotypes", recycled_args$phenotypes)}'
  )

  responses <-
    request_parallel(
      resource_urls,
      verbose = verbose,
      warnings = warnings,
      progress_bar = progress_bar
    )

  responses
}
