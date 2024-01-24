#' Retrieve variant consequences
#'
#' This function retrieves variant consequence types. For more details check
#' \href{https://m.ensembl.org/info/genome/variation/prediction/predicted_data.html}{Ensembl
#' Variation - Calculated variant consequences}.
#'
#' A rule-based approach is used to predict the effects that each allele of a
#' variant may have on each transcript. These effects are variant consequences,
#' that are catalogued as consequence terms, defined by the [Sequence
#' Ontology](http://www.sequenceontology.org/).
#'
#' See below a diagram showing the location of each display term relative to the
#' transcript structure:
#'
#' \if{html}{\figure{consequences-fs8.png}{options: width="90\%" alt="Figure: consequences-fs8.png"}}
#' \if{latex}{\figure{consequences-fs8.png}{options: width=7cm}}
#'
#' @param verbose Whether to be chatty about the underlying requests.
#' @param warnings Whether to print warnings.
#' @return A \code{\link[tibble]{tibble}}, each row being a variant consequence,
#'   of four variables:
#' \describe{
#' \item{SO_accession}{\href{http://www.sequenceontology.org/}{Sequence
#' Ontology} accession, e.g., \code{'SO:0001626'}.}
#' \item{SO_term}{\href{http://www.sequenceontology.org/}{Sequence Ontology}
#' term, e.g., \code{'incomplete_terminal_codon_variant'}.}
#' \item{label}{Display term.}
#' \item{description}{\href{http://www.sequenceontology.org/}{Sequence Ontology}
#' description.}
#' }
#'
#' @details # Ensembl REST API endpoints
#'
#' `get_variant_consequence_types` makes GET requests to
#' [/info/variation/consequence_types](https://rest.ensembl.org/documentation/info/variation_consequence_types).
#'
#' @examples
#' # Retrieve variant consequence types
#' get_variant_consequences()
#'
#' @md
#' @export
get_variant_consequences <- function(verbose = FALSE, warnings = TRUE) {
  response <- request(
    resource_url = "/info/variation/consequence_types",
    verbose = verbose,
    warnings = warnings
  )

  if (!identical(response$status, "OK")) {
    rlang::abort(
      "Could not get a successful response\n",
      "Response code was {response$response_code}."
    )
  }

  tbl <- tibble::tibble(
    SO_accession = purrr::pluck(response, "content", "SO_accession", .default = NA_character_),
    SO_term = purrr::pluck(response, "content", "SO_term", .default = NA_character_),
    label = purrr::pluck(response, "content", "label", .default = NA_character_),
    description = purrr::pluck(response, "content", "description", .default = NA_character_)
  )

  return(tbl)
}
