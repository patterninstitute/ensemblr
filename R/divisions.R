#' Retrieve Ensembl divisions
#'
#' This function retrieves Ensembl divisions. Ensembl data is split up in
#' separate databases which are loosely based on taxonomic divisions or
#' sub-groups.
#'
#' @param verbose Whether to be chatty.
#' @param warnings Whether to print warnings.
#' @return A character vector of Ensembl divisions.
#'
#' @examples
#' # Retrieve a character vector of Ensembl divisions
#' get_divisions()
#'
#' @export
get_divisions <- function(verbose = FALSE, warnings = TRUE) {

  response <- request(
    resource_url = '/info/divisions?',
    verbose = verbose,
    warnings = warnings)

  if (!identical(response$status, 'OK'))
    rlang::abort('Could not get a successful response\n',
                 'Response code was {response$response_code}.'
    )
  return(purrr::pluck(response, 'content'))
}
