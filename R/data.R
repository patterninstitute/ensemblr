#' Ensembl REST API Endpoints.
#'
#' A dataset containing the Ensembl REST API endpoints, as listed in
#' \url{https://rest.ensembl.org/}.
#'
#' @md
#'
#' @format A data frame with `r nrow(rest_api_endpoints)` rows and
#' `r ncol(rest_api_endpoints)` variables:
#' \describe{
#'   \item{section}{Section.}
#'   \item{endpoint}{Ensembl REST API endpoint.}
#'   \item{description}{A short description of the resource.}
#'   \item{last_update_date}{Time stamp of last time this dataset was
#'   downloaded from Ensembl.}
#' }
#' @source \url{https://rest.ensembl.org/}
'rest_api_endpoints'
