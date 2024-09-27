#' Are vectors recyclable?
#'
#' Are vectors recyclable?
#'
#' @param ... Vectors to be tested for recycling.
#'
#' @return A scalar logical: \code{TRUE} or \code{FALSE}.
#' @keywords internal
are_vec_recyclable <- function(...)
  !assertthat::is.error(try(vctrs::vec_recycle_common(...), silent = TRUE))

#' Pairwise combinations
#'
#' Generates pairwise combinations from the supplied vector. Never returns the
#' pairs self against self.
#'
#' @param x A character vector from which to generate the combinations.
#'
#' @return A \code{\link[tibble]{tibble}} of two columns where each row is a
#'   pairwise combination.
#' @keywords internal
pairwise_combn <- function(x) {

  # Is x NULL?
  assertthat::assert_that(
    !rlang::is_null(x),
    msg = '`x` cannot be NULL.'
  )

  # Is x empty?
  assertthat::not_empty(x)

  # Is x a character vector?
  assertthat::assert_that(
    rlang::is_character(x),
    msg = '`x` must be a character vector.'
  )

  # Does x contain NAs?
  assertthat::assert_that(assertthat::noNA(x))

  # Each column in pairs_matrix is a pairwise combination
  pairs_matrix <- utils::combn(x, m = 2)
  tbl <- tibble::tibble(x1 = pairs_matrix[1, ], x2 = pairs_matrix[2, ])
  return(tbl)
}

p <- function(param_name, value, missing = '') {

  params <- glue::glue('{param_name}={value}')
  params[value == missing] <- ''

  return(params)
}

empty_strings_to_NA <- function(df) {
  dplyr::mutate_if(df, is.character, list( ~ dplyr::na_if(., "")))
}
