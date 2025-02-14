eqtl_tbl <- function(species_name = character(),
                  ensembl_id = character(),
                  variant_id = character(),
                  tissue = character(),
                  display_consequence = character(),
                  seq_region_name = character(),
                  seq_region_start = integer(),
                  seq_region_end = integer(),
                  beta = double(),
                  pvalue = double()
                  ) {

  tibble::tibble(species_name = species_name,
                 ensembl_id = ensembl_id,
                 variant_id = variant_id,
                 tissue = tissue,
                 display_consequence = display_consequence,
                 seq_region_name = seq_region_name,
                 seq_region_start = seq_region_start,
                 seq_region_end = seq_region_end,
                 beta = beta,
                 pvalue = pvalue)

}

#' @importFrom rlang .data
to_eqtl_tbl <- function(species_name, ensembl_id, tbl) {

  tbl %>%
  tibble::as_tibble() %>%
  tidyr::pivot_wider(
    id_cols = c(
      'snp',
      'tissue',
      'display_consequence',
      'seq_region_name',
      'seq_region_start',
      'seq_region_end'
    ),
    names_from = 'statistic',
    values_from = 'value'
  ) %>%
    dplyr::mutate(seq_region_start = as.integer(.data$seq_region_start), seq_region_end = as.integer(.data$seq_region_end)) %>%
    tibble::add_column(species_name = species_name, ensembl_id = ensembl_id, .before = 1L) %>%
    dplyr::rename(variant_id = .data$snp, pvalue = .data$`p-value`)
}

#' Get eQTL details by gene ensembl identifier
#'
#' This function retrieves eQTLs, along with its genomic position, beta values
#' and p-values.
#'
#' @param ensembl_id An Ensembl stable identifier, e.g. `"ENSG00000248234378"`
#'   (or a vector thereof).
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Currently, only
#'   \code{'homo_sapiens'} (human) is supported.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of `r ncol(eqtl_tbl())`
#'   variables:
#' \describe{
#'   \item{`species_name`}{Ensembl species name: this is the name used
#'   internally by Ensembl to uniquely identify a species by name. It is the
#'   scientific name but formatted without capitalisation and spacing converted
#'   with an underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{`ensembl_id`}{An Ensembl stable identifier, e.g.
#'   \code{"ENSG00000248234378"}.}
#'   \item{`variant_id`}{Variant identifier, e.g. \code{"rs80100814"}.}
#'   \item{`tissue`}{Tissue.}
#'   \item{`display_consequence`}{Variant consequence type.}
#'   \item{`seq_region_name`}{Sequence region name (typically a chromosome name)
#'   of the variant.}
#'   \item{`seq_region_start`}{Genomic start position of the variant.}
#'   \item{`seq_region_end`}{Genomic end position of the variant.}
#'   \item{`beta`}{The effect size beta of the eQTL association analysis.}
#'   \item{`pvalue`}{The p-value of the eQTL association analysis.}
#' }
#'
#' @details # Ensembl REST API endpoints
#'
#' `get_eqtl_pval_by_gene()` makes GET requests to
#' [/eqtl/id/:species/:stable_id](https://rest.ensembl.org/documentation/info/species_id).
#'
#' @examples
#' get_eqtl_pval_by_gene('ENSG00000248378')
#'
#' @md
#' @export
# get_eqtl_pval_by_gene <-
#   function(ensembl_id,
#            species_name = 'homo_sapiens',
#            verbose = FALSE,
#            warnings = TRUE,
#            progress_bar = TRUE) {
#
#     resource_urls <- glue::glue('/eqtl/id/',
#                                 '{species_name}/{ensembl_id}')
#
#     responses <-
#       request_parallel(
#         resource_urls,
#         verbose = verbose,
#         warnings = warnings,
#         progress_bar = progress_bar
#       )
#
#     # Only keep those responses that responded successfully, i.e. with status == "OK".
#     responses_ok <- purrr::keep(responses, ~ identical(.x$status, 'OK') && !rlang::is_empty(.x$content))
#     if (rlang::is_empty(responses_ok)) return(eqtl_tbl())
#
#     return(
#       purrr::imap_dfr(
#         .x = responses_ok,
#         .f = ~ to_eqtl_tbl(
#           species_name = species_name[.y],
#           ensembl_id = ensembl_id[.y],
#           tbl = .x$content
#         )
#       )
#     )
# }
