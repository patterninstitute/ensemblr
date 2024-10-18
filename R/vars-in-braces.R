vars_in_braces <- function(x) {
  matches <- stringr::str_extract_all(x, "\\{([^}]+)\\}")
  vars <- gsub("[{}]", "", unlist(matches))

  vars
}
