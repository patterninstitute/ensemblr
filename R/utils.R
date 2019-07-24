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
