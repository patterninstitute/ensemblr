##----Re-wrote the already existing functions from `version.R`----

#' Get REST API version
#'
#' Retrieve the version of the Ensembl REST API currently in use.
#' The version format is `major.minor.point`.
#' @return A list containing the REST API version components:
#' `major`, `minor`, and `point`.
#' @source \url{https://github.com/Ensembl/ensembl-rest/wiki/API-Versioning}
#' @examples
#' \dontrun{
#' rest_version <- get_rest_version()
#' print(rest_version)
#' }
#' @export
get_rest_version2 <- function() {
  res <- "info/rest"
  version <- ensemblr:::get(res)[[1]] |>
    httr2::resp_body_json()
  version$release
}

#' Get data version
#'
#' Retreive the version(s) of the Ensembl data that the REST API is accessing.
#' @return A numeric vector of data release versions.
#' @source \url{https://github.com/Ensembl/ensembl-rest/wiki/API-Versioning}
#' @examples
#' \dontrun{
#' data_version <- get_data_version()
#' print(data_version)
#' }
#' @export
get_data_version2 <- function() {
  res <- "info/data"
  version <- ensemblr:::get(res)[[1]] |>
    httr2::resp_body_json()
  version$release |> as.numeric()
}

#' Get API software version
#'
#' Retreive the version of the Ensembl software the REST API is using.
#' @return A numeric value representing the software version.
#' @source \url{https://github.com/Ensembl/ensembl-rest/wiki/API-Versioning}
#' @examples
#' \dontrun{
#' software_version <- get_software_version()
#' print(software_version)
#' }
#' @export
get_software_version2 <- function() {
  res <- "info/data"
  version <- ensemblr:::get(res)[[1]] |>
    httr2::resp_body_json()
  version$release |> as.numeric()
}

#' Get Ensembl REST versions
#'
#' Retreive the versions of the different entities involved in the
#' REST API requests. When accessing the Ensembl REST API, you are actually
#' accessing three interconnected entities:
#' \itemize{
#' \item Ensembl databases (\code{data}).
#' \item Perl API (\code{software}).
#' \item REST API (\code{rest}).
#' }
#' \figure{ensembl_api_versioning_wo_fonts.svg}
#'
#' @return A named list of three elements: \code{data}, \code{software} and
#'   \code{rest}.
#'
#' @examples
#' # Get the versions of the different entities involved in the REST API
#' # requests.
#' get_versioning()
#'
#' @export
get_versioning2 <- function() {
  data_version <- get_data_version2()
  software_version <- get_software_version2()
  rest_version <- get_rest_version2()

  api_versions <- list(data = data_version,
                       software = software_version,
                       rest = rest_version)

  return(api_versions)
}
