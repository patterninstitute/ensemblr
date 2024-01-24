#' Retrieve the Perl API version
#'
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @return A scalar integer vector with the Perl API version.
#'
#' @export
get_software_version <- function(verbose = FALSE, warnings = TRUE) {
  response <- request(
    resource_url = "/info/software?",
    verbose = verbose,
    warnings = warnings
  )

  return(purrr::pluck(response,
    "content",
    "release",
    .default = NA_integer_
  ))
}

#' Retrieve the current version of the Ensembl REST API
#'
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @return A scalar character vector with Ensembl REST API version.
#'
#' @export
get_rest_version <- function(verbose = FALSE, warnings = TRUE) {
  response <- request(
    resource_url = "/info/rest?",
    verbose = verbose,
    warnings = warnings
  )

  return(purrr::pluck(response,
    "content",
    "release",
    .default = NA_character_
  ))
}

#' Retrieve the data release version(s) available on the Ensembl REST server.
#'
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @return An integer vector of release version(s).
#'
#' @export
get_data_versions <- function(verbose = FALSE, warnings = TRUE) {
  response <- request(
    resource_url = "/info/data?",
    verbose = verbose,
    warnings = warnings
  )

  return(purrr::pluck(response,
    "content",
    "releases",
    .default = NA_integer_
  ))
}

#' Retrieve Ensembl REST versions
#'
#' This function gets the versions of the different entities involved in the
#' REST API requests. When accessing the Ensembl REST API, you are actually
#' accessing three interconnected entities:
#' \itemize{
#' \item Ensembl databases (\code{data}).
#' \item Perl API (\code{software}).
#' \item REST API (\code{rest}).
#' }
#' \figure{ensembl_api_versioning_wo_fonts.svg}
#'
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @return A named list of three elements: \code{data}, \code{software} and
#'   \code{rest}.
#'
#' @examples
#' # Get the versions of the different entities involved in the REST API
#' # requests.
#' get_versioning()
#'
#' @export
get_versioning <- function(verbose = FALSE, warnings = TRUE) {
  # Ensembl data release version(s)
  data_version <-
    get_data_versions(verbose = verbose, warnings = warnings)

  # Ensembl internal Perl API version
  software_version <-
    get_software_version(verbose = verbose, warnings = warnings)

  # Ensembl REST API version
  rest_version <-
    get_rest_version(verbose = verbose, warnings = warnings)

  api_versions <- list(
    data = data_version,
    software = software_version,
    rest = rest_version
  )

  return(api_versions)
}
