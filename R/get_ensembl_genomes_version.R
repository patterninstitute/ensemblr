#' Get Ensembl Genomes version
#'
#' Returns the Ensembl Genomes version of the databases backing this service.
#'
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#'
#' @return An integer value: the Ensembl Genomes version.
#'
#' @examples
#' get_ensembl_genomes_version()
#'
#' @export
get_ensembl_genomes_version <- function(verbose = FALSE, warnings = TRUE) {
  response <- request("/info/eg_version", verbose = verbose, warnings = warnings)

  if (identical(response$status, "OK")) {
    return(response$content$version)
  } else {
    return(NA_integer_)
  }
}
