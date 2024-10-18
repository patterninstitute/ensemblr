#' Create genomic range strings
#'
#' This function converts three vectors: \code{chr}, \code{start}, and \code{end}
#' to strings of the form \{chr\}:\{start\}..\{end\}.
#'
#' @param chr A character vector of chromosome names.
#' @param start An integer vector of start positions.
#' @param end An integer vector of end positions.
#' @param starting_position_index Use this argument to indicate if the positions
#' are 0-based (\code{0L}) or 1-based (\code{1L}). This value is used to check
#' if positions are equal or above this number.
#' @return Returns a character vector whose strings are genomic ranges of the
#' form \{chr\}:\{start\}..\{end\}.
#'
#' @examples
#' genomic_range("1", 10000L, 20000L) # Returns "1:10000..20000"
#'
#' @export
genomic_range <- function(chr, start, end, starting_position_index = 1L) {

  if (!(identical(starting_position_index, 0L) || identical(starting_position_index, 1L)))
    stop("starting_position_index must be either 0L or 1L.")

  if (!is.character(chr))
    stop("chr needs to a character vector.")

  if (identical(length(chr), 0L))
    stop("chr is empty, must have at least one chromosome name.")

  if (!is.integer(start))
    stop("start needs to an integer vector.")

  if (identical(length(start), 0L))
    stop("start is empty, must have at least one start position.")

  if (!is.integer(end))
    stop("end needs to an integer vector.")

  if (identical(length(end), 0L))
    stop("end is empty, must have at least one end position.")

  n_chr <- length(chr)
  n_start <- length(start)
  n_end <- length(end)
  if (!(identical(n_start, n_end) && identical(n_start, n_chr))) # identical(n_end, n_chr) == TRUE follows.
    stop("chr, start and end vectors should be of same length: ",
         "len(chr) = ", n_chr, ", ",
         "len(start) = ", n_start, ", and ",
         "len(end) = ", n_end, ".")

  is_start_below_starting_pos <- start < starting_position_index
  if (any(is_start_below_starting_pos))
    stop("All start positions must be greater than ", starting_position_index, ", these are not: ",
         concatenate::cc_and(start[is_start_below_starting_pos], oxford = TRUE), ".")

  is_end_below_starting_pos <- end < starting_position_index
  if (any(is_end_below_starting_pos))
    stop("All end positions must be greater than ", starting_position_index, ", these are not: ",
         concatenate::cc_and(end[is_end_below_starting_pos], oxford = TRUE), ".")

  # Generate genomic ranges strings.
  gen_ranges <- sprintf("%s:%d..%d", chr, start, end)

  # When is start greater than end? (should not happen.)
  start_gr_end <- start > end
  if (any(start_gr_end))
    stop("start positions cannot be larger than end positions: ",
         concatenate::cc_and(gen_ranges[start_gr_end], oxford = TRUE), ".")

  # Check that all genomic ranges' strings conform to criteria of is_genomic_range.
  is_gen_ranges <- is_genomic_range(gen_ranges)
  if (!all(is_gen_ranges))
    stop("The following are not well-formed genomic ranges: ",
         concatenate::cc_and(gen_ranges[!is_gen_ranges], oxford = TRUE), ".")

  return(gen_ranges)

}

is_genomic_range <- function(genomic_range)
  stringr::str_detect(genomic_range, '\\w+:\\d+\\.\\.\\d+')

split_genomic_range <- function(genomic_range) {

  split_coordinates <- stringr::str_match(genomic_range,
                                          '^(\\w+):(\\d+)\\.\\.(\\d+)$')[, -1, drop = FALSE]

  colnames(split_coordinates) <- c('chromosome', 'start', 'end')
  return(split_coordinates)
}
