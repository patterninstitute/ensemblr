eqtl_tissue_tbl <- function(
  species_name = character(),
  tissue = character()
) {
  tbl <- tibble::tibble(
    species_name = species_name,
    tissue = tissue
  )

  return(tbl)
}

json_list_to_eqtl_tissue_tbl <- function(species_name, json_list) {

  tissues <- names(json_list)
  tbl <- eqtl_tissue_tbl(
    species_name = species_name,
    tissue = tissues
  )

  # Drop rows if all columns except species_name are NA
  tbl2 <- tidyr::drop_na(tbl, -species_name)

  tissue <- rlang::sym('tissue')
  # Sort alphabetically by tissue
  tbl3 <- dplyr::arrange(tbl2, species_name, !!tissue)
  return(tbl3)
}

#' Get tissues in the eQTL database
#'
#' This function retrieves all tissues in the eQTL database.
#'
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Currently, only human
#'   \code{'homo_sapiens'} is available.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @details # Ensembl REST API endpoints
#'
#' `get_eqtl_tissues()` makes GET requests to
#' [/eqtl/tissue/:species/](https://rest.ensembl.org/documentation/info/tissues).
#'
#' @examples
#' get_eqtl_tissues()
#'
#' @md
#' @export
# get_eqtl_tissues <- function(
#   species_name = 'homo_sapiens',
#   verbose = FALSE,
#   warnings = TRUE,
#   progress_bar = TRUE
#   ) {
#
#   # Assert species_name argument.
#   assert_species_name(species_name)
#   # Assert verbose argument.
#   assertthat::assert_that(assertthat::is.flag(verbose))
#   # Assert warnings argument.
#   assertthat::assert_that(assertthat::is.flag(warnings))
#   # Assert progress_bar argument.
#   assertthat::assert_that(assertthat::is.flag(progress_bar))
#
#   resource_urls <- glue::glue(
#     '/eqtl/tissue/',
#     '{species_name}/'
#   )
#
#   # Usually we'd use purrr::map here but we opted for plyr::llply
#   # for a no frills alternative with progress bar support.
#   # progress <- dplyr::if_else(progress_bar && interactive(), 'text', 'none')
#   # responses <- plyr::llply(
#   #   .data = resource_urls,
#   #   .fun = request,
#   #   verbose = verbose,
#   #   warnings = warnings,
#   #   .progress = progress)
#   responses <-
#     request_parallel(
#       resource_urls,
#       verbose = verbose,
#       warnings = warnings,
#       progress_bar = progress_bar
#     )
#
#   # Only keep those responses that responded successfully, i.e. with status == "OK".
#   responses_ok <- purrr::keep(responses, ~ identical(.x$status, 'OK'))
#
#   # If none of the responses were successful then return an empty linkage
#   # disequilibrium tibble.
#   if (rlang::is_empty(responses_ok)) return(eqtl_tissue_tbl())
#
#   return(
#     purrr::imap_dfr(
#       .x = responses_ok,
#       .f = ~ json_list_to_eqtl_tissue_tbl(
#         species_name = species_name[.y],
#         json_list = .x$content
#       )
#     )
#   )
#
# }
