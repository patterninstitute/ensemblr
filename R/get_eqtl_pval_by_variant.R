eqtl_tbl2 <- function(species_name = character(),
                      variant_id = character(),
                      ensembl_id = character(),
                      tissue = character(),
                      beta = double(),
                      pvalue = double()) {
  tibble::tibble(
    species_name = species_name,
    variant_id = variant_id,
    ensembl_id = ensembl_id,
    tissue = tissue,
    beta = beta,
    pvalue = pvalue
  )

}

#' @importFrom rlang .data
to_eqtl_tbl2 <- function(species_name, variant_id, tbl) {

  tbl %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(
      id_cols = c(
        'gene',
        'tissue'
      ),
      names_from = 'statistic',
      values_from = 'value'
    ) %>%
    tibble::add_column(species_name = species_name, variant_id = variant_id, .before = 1L) %>%
    dplyr::rename(ensembl_id = .data$gene, pvalue = .data$`p-value`)
}

#' Get eQTL details by variant identifier
#'
#' This function retrieves genes (Ensembl identifier) along with beta values and
#' p-values for the eQTL association.
#'
#' @param variant_id A variant identifier, e.g. \code{"rs80100814"}.
#' @param species_name The species name, i.e., the scientific name, all letters
#'   lowercase and space replaced by underscore. Currently, only
#'   \code{'homo_sapiens'} (human) is supported.
#' @param verbose Whether to be verbose about the http requests and respective
#'   responses' status.
#' @param warnings Whether to show warnings.
#' @param progress_bar Whether to show a progress bar.
#'
#' @return A \code{\link[tibble]{tibble}} of `r ncol(eqtl_tbl2())`
#'   variables:
#' \describe{
#'   \item{`species_name`}{Ensembl species name: this is the name used
#'   internally by Ensembl to uniquely identify a species by name. It is the
#'   scientific name but formatted without capitalisation and spacing converted
#'   with an underscore, e.g., \code{'homo_sapiens'}.}
#'   \item{`variant_id`}{Variant identifier, e.g. \code{"rs80100814"}.}
#'   \item{`ensembl_id`}{An Ensembl stable identifier, e.g.
#'   \code{"ENSG00000248234378"}.}
#'   \item{`tissue`}{Tissue.}
#'   \item{`beta`}{The effect size beta of the eQTL association analysis.}
#'   \item{`pvalue`}{The p-value of the eQTL association analysis.}
#' }
#'
#' @details # Ensembl REST API endpoints
#'
#' `get_eqtl_pval_by_variant()` makes GET requests to
#' [/eqtl/variant_name/:species/:variant_name](https://rest.ensembl.org/documentation/info/species_variant).
#'
#' @examples
#' get_eqtl_pval_by_variant('rs80100814')
#'
#' @md
#' @export
get_eqtl_pval_by_variant <-
  function(variant_id,
           species_name = 'homo_sapiens',
           verbose = FALSE,
           warnings = TRUE,
           progress_bar = TRUE) {

    resource_urls <- glue::glue('/eqtl/variant_name/',
                                '{species_name}/{variant_id}')

    responses <-
      request_parallel(
        resource_urls,
        verbose = verbose,
        warnings = warnings,
        progress_bar = progress_bar
      )

    # Only keep those responses that responded successfully, i.e. with status == "OK".
    responses_ok <- purrr::keep(responses, ~ identical(.x$status, 'OK') && !rlang::is_empty(.x$content))
    if (rlang::is_empty(responses_ok)) return(eqtl_tbl2())

    return(
      purrr::imap_dfr(
        .x = responses_ok,
        .f = ~ to_eqtl_tbl2(
          species_name = species_name[.y],
          variant_id = variant_id[.y],
          tbl = .x$content
        )
      )
    )
  }
