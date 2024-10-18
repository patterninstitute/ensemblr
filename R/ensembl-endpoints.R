# These functions will perform validation on input parameters that are tailored
# to each specific endpoint, and return already an R object that is sensible
# for the type of information being returned by the endpoint
# -------------------------------------------------------- #
# Endpoints ====
## Comparative Genomics =====

#' Get cafe gene tree by id
#'
#' Retrieves a cafe tree of the gene tree using the gene tree stable identifier
#'
#' @param id A string representing the gene tree stable identifier.
#'
#' @return A list of parsed JSON responses containing the cafe tree
#' for the provided gene tree stable identifier.
#'
#' @note
#' See more about the implemented endpoint [get_cafe_genetree_by_id()]
#' on the following [GET cafe/genetree/id/:id](https://rest.ensembl.org/documentation/info/cafe_tree)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_cafe_genetree_by_id("ENSGT00390000003602")
#'
get_cafe_genetree_by_id <- function(id) {
  if (missing(id)) {
    stop("The 'id' parameter is required.")
  }
  response <- get(
    res = glue::glue("/cafe/genetree/id/{id}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get cafe gene tree by symbol
#'
#' Retrieves the cafe tree of the gene tree that contains the gene identified
#' by a symbol
#'
#' @param species A string representing the species name (e.g.,
#' "homo_sapiens").
#' @param symbol A string representing the gene symbol (e.g., "BRCA2").
#'
#' @return A list of parsed JSON responses containing the cafe tree
#' for the provided species and gene symbol.
#'
#' @note
#' See more about the implemented endpoint [get_cafe_genetree_by_symbol()]
#' on the following [GET cafe/genetree/member/symbol/:species/:symbol](https://rest.ensembl.org/documentation/info/cafe_tree_member_symbol)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_cafe_genetree_by_symbol("homo_sapiens", "BRCA2")
get_cafe_genetree_by_symbol <- function(species, symbol) {
  if (missing(species) || missing(symbol)) {
    stop("Both 'species' and 'symbol' parameters are required.")
  }
  response <- get(
    res = glue::glue("/cafe/genetree/member/symbol/{species}/{symbol}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get cafe gene tree by species id
#'
#' Retrieves the cafe tree of the gene tree that contains the gene/transcript/translation stable identifier in the given species
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param id A string representing the gene, transcript, or translation stable identifier.
#'
#' @return A list of parsed JSON responses containing the cafe tree for the provided species and stable identifier.
#'
#' @note
#' See more about the implemented endpoint [get_cafe_genetree_by_species_id()]
#' on the following [GET cafe/genetree/member/id/:species/:id](https://rest.ensembl.org/documentation/info/cafe_tree_species_member_id)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_cafe_genetree_by_species_id("homo_sapiens", "ENST00000380152")
get_cafe_genetree_by_species_id <- function(species, id) {
  if (missing(species) || missing(id)) {
    stop("Both 'species' and 'id' parameters are required.")
  }
  response <- get(
    res = glue::glue("/cafe/genetree/member/id/{species}/{id}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get gene tree by id
#'
#' Retrieves a gene tree for a gene tree stable identifier
#'
#' @param id A string representing the gene tree stable identifier.
#'
#' @return A list of parsed JSON responses containing the gene tree for the provided gene tree stable identifier.
#'
#' @note
#' See more about the implemented endpoint [get_genetree_by_id()]
#' on the following [GET genetree/id/:id](https://rest.ensembl.org/documentation/info/genetree)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genetree_by_id("ENSGT00390000003602")
get_genetree_by_id <- function(id) {
  if (missing(id)) {
    stop("The 'id' parameter is required.")
  }
  response <- get(
    res = glue::glue("/genetree/id/{id}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get gene tree by symbol
#'
#' Retrieves the gene tree that contains the gene identified by a symbol
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param symbol A string representing the gene symbol (e.g., "BRCA2").
#'
#' @return A list of parsed JSON responses containing the gene tree
#' for the provided species and gene symbol.
#'
#' @note
#' See more about the implemented endpoint [get_genetree_by_symbol()]
#' on the following [GET genetree/member/symbol/:species/:symbol](https://rest.ensembl.org/documentation/info/genetree_member_symbol)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genetree_by_symbol("homo_sapiens", "BRCA2")
get_genetree_by_symbol <- function(species, symbol) {
  if (missing(species) || missing(symbol)) {
    stop("Both 'species' and 'symbol' parameters are required.")
  }
  response <- get(
    res = glue::glue("/genetree/member/symbol/{species}/{symbol}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get gene tree by species id
#'
#' Retrieves the gene tree that contains the gene/transcript/translation
#' stable identifier in the given species
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param id A string representing the gene, transcript, or translation stable
#' identifier.
#'
#' @return A list of parsed JSON responses containing the gene tree
#' for the provided species and stable identifier.
#'
#' @note
#' See more about the implemented endpoint [get_genetree_by_species_id()]
#' on the following [GET genetree/member/id/:species/:id](https://rest.ensembl.org/documentation/info/genetree_species_member_id)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genetree_by_species_id("homo_sapiens", "ENST00000380152")
get_genetree_by_species_id <- function(species, id) {
  if (missing(species) || missing(id)) {
    stop("Both 'species' and 'id' parameters are required.")
  }
  response <- get(
    res = glue::glue("/genetree/member/id/{species}/{id}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get alignment by region
#'
#' Retrieves genomic alignments as separate blocks based on a region and species
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param region A string representing the genomic region (e.g., "3:1000-2000").
#'
#' @return A list of parsed JSON responses containing the genomic alignments
#' for the provided species and region.
#'
#' See more about the implemented endpoint [get_alignment_by_region()]
#' on the following [GET alignment/region/:species/:region](https://rest.ensembl.org/documentation/info/genomic_alignment_region)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_alignment_by_region("homo_sapiens", "3:1000-2000")
get_alignment_by_region <- function(species, region) {
  warning("This function is stil under-develop. If you run it now
          it will return you an error.")
  if (missing(species) || missing(region)) {
    stop("Both 'species' and 'region' parameters are required.")
  }
  # response <- get(
  #   res = glue::glue("/alignment/region/{species}/{region}"),
  #   .headers = req_headers(content_type = "application/json")
  # )
}

#' Get homologous by species id
#'
#' Retrieves homology information (orthologs) by species and Ensembl gene ID
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param id A string representing the Ensembl gene ID.
#'
#' @return A list of parsed JSON responses containing homology information
#' for the provided species and Ensembl gene ID.
#'
#' See more about the implemented endpoint [get_homology_by_species_id()]
#' on the following [GET homology/id/:species/:id](https://rest.ensembl.org/documentation/info/homology_species_gene_id)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_homology_by_species_id("homo_sapiens", "ENSG00000157764")
get_homology_by_species_id <- function(species, id) {
  if (missing(species) || missing(id)) {
    stop("Both 'species' and 'id' parameters are required.")
  }
  response <- get(
    res = glue::glue("/homology/id/{species}/{id}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get homologous by symbol
#'
#' Retrieves homology information (orthologs) by species and symbol
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param symbol A string representing the gene symbol (e.g., "BRCA2").
#'
#' @return A list of parsed JSON responses containing homology information
#' for the provided species and gene symbol.
#'
#' See more about the implemented endpoint [get_homology_by_symbol()]
#' on the following [GET homology/symbol/:species/:symbol](https://rest.ensembl.org/documentation/info/homology_symbol)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_homology_by_symbol("homo_sapiens", "BRCA2")
get_homology_by_symbol <- function(species, symbol) {
  if (missing(species) || missing(symbol)) {
    stop("Both 'species' and 'symbol' parameters are required.")
  }
  response <- get(
    res = glue::glue("/homology/symbol/{species}/{symbol}"),
    .headers = req_headers(content_type = "application/json")
  )
}

# -------------------------------------------------------- #
## Cross References ====

#' Get external linked references by symbol
#'
#' Looks up an external symbol and returns all Ensembl objects linked to it
#'
#' This can be a display name for a gene/transcript/translation, a synonym,
#' or an externally linked reference.
#' If a gene's transcript is linked to the supplied symbol, the service will
#' return both gene and transcript (it supports transient links).
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param symbol A string representing the external symbol (e.g., "BRCA2").
#'
#' @return A list of parsed JSON responses containing Ensembl objects linked
#' to the provided external symbol.
#'
#' See more about the implemented endpoint [get_xrefs_by_symbol()]
#' on the following [GET xrefs/symbol/:species/:symbol](https://rest.ensembl.org/documentation/info/xref_external)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_xrefs_by_symbol("homo_sapiens", "BRCA2")
get_xrefs_by_symbol <- function(species, symbol) {
  if (missing(species) || missing(symbol)) {
    stop("Both 'species' and 'symbol' parameters are required.")
  }
  response <- get(
    res = glue::glue("/xrefs/symbol/{species}/{symbol}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get external linked references by id
#'
#' Performs lookups of Ensembl Identifiers and retrieves their external
#' references in other databases
#'
#' @param id A string representing the Ensembl Identifier (e.g., "ENSG00000157764").
#'
#' @return A list of parsed JSON responses containing external references
#' for the provided Ensembl identifier.
#'
#' See more about the implemented endpoint [get_xrefs_by_id()]
#' on the following [GET xrefs/id/:id](https://rest.ensembl.org/documentation/info/xref_id)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_xrefs_by_id("ENSG00000157764")
get_xrefs_by_id <- function(id) {
  if (missing(id)) {
    stop("The 'id' parameter is required.")
  }
  response <- get(
    res = glue::glue("/xrefs/id/{id}"),
    .headers = req_headers(content_type = "application/json")
  )
}

#' Get external linked references by name
#'
#' Performs a lookup based upon the primary accession or display label
#' of an external reference
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param name A string representing the primary accession or display label
#' of the external reference.
#'
#' @return A list of parsed JSON responses containing information about
#' the provided external reference.
#'
#' See more about the implemented endpoint [get_xrefs_by_name()]
#' on the following [GET xrefs/name/:species/:name](https://rest.ensembl.org/documentation/info/xref_name)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_xrefs_by_name("homo_sapiens", "P38398")
get_xrefs_by_name <- function(species, name) {
  if (missing(species) || missing(name)) {
    stop("Both 'species' and 'name' parameters are required.")
  }
  response <- get(
    res = glue::glue("/xrefs/name/{species}/{name}"),
    .headers = req_headers(content_type = "application/json")
  )
}

# -------------------------------------------------------- #
## Information ====

# -------------------------------------------------------- #
## Linkage Disequilibrium ====

# -------------------------------------------------------- #
## Lookup ====

# -------------------------------------------------------- #
## Mapping ====

# -------------------------------------------------------- #
## Ontologies and taxonomy ====

# -------------------------------------------------------- #
## Overlap ====

# -------------------------------------------------------- #
## Phenotype annotations ====

# -------------------------------------------------------- #
## Regulation ====

# -------------------------------------------------------- #
## Transcript Haplotypes ====

# -------------------------------------------------------- #
## VEP ====

# -------------------------------------------------------- #
## Variation ====

# -------------------------------------------------------- #
## Variation GA4GH ====
