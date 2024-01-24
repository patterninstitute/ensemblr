#' @importFrom tibblify tspec_object
#' @importFrom tibblify tib_df
#' @importFrom tibblify tib_row
#' @importFrom tibblify tib_dbl
#' @importFrom tibblify tib_chr
#' @importFrom tibblify tib_int
spec_homology_info_symbol <- function() {
  tspec_object(
    tib_df(
      "data",
      tib_df(
        "homologies",
        tib_dbl("dn_ds"),
        tib_row(
          "target",
          tib_chr("protein_id"),
          tib_chr("species"),
          tib_chr("cigar_line"),
          tib_chr("align_seq"),
          tib_chr("id"),
          tib_int("taxon_id"),
          tib_dbl("perc_pos"),
          tib_dbl("perc_id"),
        ),
        tib_chr("method_link_type"),
        tib_row(
          "source",
          tib_int("taxon_id"),
          tib_dbl("perc_pos"),
          tib_chr("id"),
          tib_dbl("perc_id"),
          tib_chr("cigar_line"),
          tib_chr("species"),
          tib_chr("protein_id"),
          tib_chr("align_seq"),
        ),
        tib_chr("type"),
        tib_chr("taxonomy_level"),
      ),
      tib_chr("id"),
    ),
  )
}

get_homology_info_by_symbol <- function(species_name, symbol = NULL) {
  resource <- glue::glue("/homology/symbol/{species_name}/{symbol}")
  lst <- get2(resource, `content-type` = "application/json")

  purrr::map(lst, ~ tibblify::tibblify(.x, spec_homology_info_symbol()))
}
