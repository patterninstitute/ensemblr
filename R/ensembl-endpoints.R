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
#' @param callback String \emph{(optional)} Name of the callback subroutine
#' to be returned by the requested JSONP response. Required ONLY when using
#' JSONP as the serialisation method. Please
#' see also [the user guide](http://github.com/Ensembl/ensembl-rest/wiki).
#' @param compara String \emph{(optional)} Name of the compara database to use.
#' Multiple comparas exist on a server for separate species divisions.
#' Default is "vertebrates".
#' @param nh_format String \emph{(optional)} The format of a NH (New Hampshire)
#' request. Available only with the default setting to allow us to return
#' the cafe tree with Taxa names appended with number of members
#' and the p_value. Example: "homo_sapiens_3_0.123" where 3 is the number
#' of members and 0.123 is the p value.
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
#' get_cafe_genetree_by_id("ENSGT00390000003602",
#'                         callback = "randomlygeneratedname")
#' get_cafe_genetree_by_id("ENSGT00390000003602",
#'                         compara = "vertebrates")
#' get_cafe_genetree_by_id("ENSGT00390000003602",
#'                         nh_format = "homo_sapiens_3_0.123")
#'
get_cafe_genetree_by_id <- function(id, callback = "myrandomfunctionname",
                                    compara = "vertebrates",
                                    nh_format = "simple") {
  if (missing(id)) {
    stop("The 'id' parameter is required.")
  }

  cat("\n`compara` seems to not be working for this endpoint.
  Hence this specific parameter is ignored for the moment.
  Here is more detail on the paramater:
  https://rest.ensembl.org/documentation/info/cafe_tree
    \n")

  if (!is.null(callback)) {
    response <- get(res = "cafe/genetree/id/{id}", id = id,
      nh_format = nh_format,
      .headers = req_headers(content_type = "application/json"))
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get cafe gene tree by symbol
#'
#' Retrieves the cafe tree of the gene tree that contains the gene identified
#' by a symbol
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
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
  response <- get(res = "/cafe/genetree/member/symbol/{species}/{symbol}",
    species = species, symbol = symbol,
    symbol = symbol, .headers = req_headers(content_type = "application/json")
  )

  response
}

#' Get cafe gene tree by species id
#'
#' Retrieves the cafe tree of the gene tree that contains the
#' gene/transcript/translation stable identifier in the given species
#'
#' @param species A string representing the species name (e.g., "homo_sapiens").
#' @param id A string representing the gene, transcript, or translation
#' stable identifier.
#'
#' @return A list of parsed JSON responses containing the cafe tree for
#' the provided species and stable identifier.
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
  response <- get(res = "/cafe/genetree/member/id/{species}/{id}",
    species = species, id = id,
    .headers = req_headers(content_type = "application/json")
  )
  response
}

#' Get gene tree by id
#'
#' Retrieves a gene tree for a gene tree stable identifier
#'
#' @param id A string representing the gene tree stable identifier.
#'
#' @return A list of parsed JSON responses containing the gene tree for
#' the provided gene tree stable identifier.
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
  response <- get(res = "/genetree/id/{id}", id = id,
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
  response <- get(res = "/genetree/member/symbol/{species}/{symbol}",
    species = species, symbol = symbol,
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
  response <- get(res = "/genetree/member/id/{species}/{id}",
    species = species, id = id,
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
  # response <- get(res = "/alignment/region/{species}/{region}",
  #   species = species, region = region,
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
  response <- get(res = "/homology/id/{species}/{id}",
    species = species, id = id,
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
  response <- get("/homology/symbol/{species}/{symbol}",
    species = species, symbol = symbol,
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
  response <- get(res = "/xrefs/symbol/{species}/{symbol}",
    species = species, symbol = symbol,
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
  response <- get(res = "/xrefs/id/{id}",
    id = id,
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
  response <- get(res = "/xrefs/name/{species}/{name}",
    species = species, name = name,
    .headers = req_headers(content_type = "application/json")
  )
}

# -------------------------------------------------------- #
## Information ====

#' Get the names of analyses involved in generating Ensembl data
#'
#' Retrieves a list of analysis names associated with generating Ensembl
#' data for a given species.
#'
#' @param species A string representing the species name or alias
#' (e.g., "homo_sapiens").
#' @param callback (Optional) A string representing the name of the callback
#' subroutine for JSONP responses.
#'
#' @return A list of analysis names related to the specified species.
#'
#' See more about the implemented endpoint [get_analysis_info()]
#' on the following [GET info/analysis/:species](https://rest.ensembl.org/documentation/info/analysis)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_analysis_info("homo_sapiens")
#' get_analysis_info("homo_sapiens", callback = "randomlygeneratedname")
get_analysis_info <- function(species, callback = "randomlygeneratedname") {
  if (missing(species)) {
    stop("'species' parameter is required.")
  }
  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(species)) query_params$species <- species
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <-
      do.call(get,
              c(list(
                res = "/info/analysis/{species}",
                .headers = headers),
                query_params)
      )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get the available assemblies for a species
#'
#' Retrieves a list of available assemblies for a given species,
#' including toplevel sequences, chromosomes, and optionally cytogenetic
#' bands and synonyms.
#'
#' @param species A string representing the species name or alias
#' (e.g., "homo_sapiens").
#' @param bands (Optional) A boolean (0 or 1) indicating whether to
#' include karyotype band information. Default is 0.
#' @param synonyms (Optional) A boolean (0 or 1) indicating whether to
#' include information about known synonyms. Default is 0.
#' @param callback (Optional) A string representing the name of the callback
#' subroutine for JSONP responses.
#'
#' @return A list of parsed JSON responses containing information about
#' the available assemblies for the specified species.
#'
#' See more about the implemented endpoint [get_assembly_info()]
#' on the following [GET info/assembly/:species](https://rest.ensembl.org/documentation/info/assembly_info)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_assembly_info("homo_sapiens")
#' get_assembly_info("homo_sapiens", bands = 1)
#' get_assembly_info("homo_sapiens", synonyms = 1,
#'   callback = "randomlygeneratedname")
get_assembly_info <- function(species, bands = 0, synonyms = 0,
                              callback = "randomlygeneratedname") {
  if (missing(species)) {
    stop("'species' parameter is required.")
  }

  if (!is.null(callback)) {
    response <- get(res = "/info/assembly/{species}", species = species,
      bands = bands, synonyms = synonyms,
      .headers = req_headers(content_type = "application/json"),
      .params = params)
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get information about a specific toplevel sequence region for a species
#'
#' Retrieves information about the specified toplevel sequence region for
#' a given species, with optional details on karyotype bands and synonyms.
#'
#' @param species A string representing the species name or alias (e.g., "homo_sapiens").
#' @param region_name A string representing the name of the toplevel sequence region (e.g., "X").
#' @param bands (Optional) A boolean (0 or 1) indicating whether to include
#' karyotype band information. Default is 0.
#' @param synonyms (Optional) A boolean (0 or 1) indicating whether to include
#' information about known synonyms. Default is 0.
#' @param callback (Optional) A string representing the name of the callback
#' subroutine for JSONP responses.
#'
#' @return A list of parsed JSON responses containing information about
#' the specified sequence region for the given species.
#'
#' See more about the implemented endpoint [get_region_info()]
#' on the following [GET info/assembly/:species/:region_name](https://rest.ensembl.org/documentation/info/assembly_stats)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_region_info("homo_sapiens", "X")
#' get_region_info("homo_sapiens", "X", bands = 1)
#' get_region_info("homo_sapiens", "X", synonyms = 1, callback = "randomlygeneratedname")
get_region_info <- function(species, region_name, bands = 0, synonyms = 0,
                            callback = "randomlygeneratedname") {
  if (missing(species) || missing(region_name)) {
    stop("'species' and 'region_name' parameters are required.")
  }

  if (!is.null(callback)) {
    response <- get(
      res = "/info/assembly/{species}/{region_name}",
      species = species, region_name = region_name,
      bands = bands, synonyms = synonyms,
      .headers = req_headers(content_type = "application/json"))
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get the functional classifications of gene models for a species
#'
#' Retrieves the list of functional classifications (biotypes) of gene models
#' that Ensembl associates with a particular species.
#' Useful for restricting the type of genes/transcripts retrieved by other endpoints.
#'
#' @param species A string representing the species name or alias (e.g., "homo_sapiens").
#' @param callback (Optional) A string representing the name of the callback subroutine
#' for JSONP responses.
#'
#' @return A list of parsed JSON responses containing the biotypes for the specified species.
#'
#' See more about the implemented endpoint [get_biotypes()]
#' on the following [GET info/biotypes/:species](https://rest.ensembl.org/documentation/info/biotypes)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_biotypes("homo_sapiens")
#' get_biotypes("homo_sapiens", callback = "randomlygeneratedname")
get_biotypes <- function(species, callback = "randomlygeneratedname") {
  if (missing(species)) {
    stop("'species' parameter is required.")
  }

  if (!is.null(callback)) {
    response <- get(
      res = "/info/biotypes/{species}", species = species,
      .headers = req_headers(content_type = "application/json"))
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get properties of biotypes within a group
#'
#' Retrieves a list of available biotype groups or, if a group is specified,
#' the properties of biotypes within that group.
#' Optionally, the object type (gene or transcript) can be used to filter the results.
#'
#' @param group (Optional) A string representing the biotype group (e.g., "coding").
#' If not provided, the available biotype groups are returned.
#' @param object_type (Optional) A string specifying the object type ("gene" or "transcript").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#'
#' @return A list of parsed JSON responses containing the properties of biotypes within
#' the specified group or all biotype groups if no group is provided.
#'
#' See more about the implemented endpoint [get_biotypes_groups()]
#' on the following [GET info/biotypes/groups/:group/:object_type](https://rest.ensembl.org/documentation/info/biotypes_groups)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_biotypes_groups()
#' get_biotypes_groups(group = "coding")
#' get_biotypes_groups(group = "coding", object_type = "gene")
#' get_biotypes_groups(group = "coding", object_type = "gene", callback = "randomlygeneratedname")
get_biotypes_groups <- function(group = '', object_type = '',
                                callback = "randomlygeneratedname") {
  # TODO: Create an issue:
  # To handle the case in which `group` parameter is null or empty
  # while `object_type` is not null nor empty.

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(group)) query_params$group <- group
    if (!is.null(object_type)) query_params$object_type <- object_type
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <-
      do.call(get,
              c(list(
                res = "/info/biotypes/groups/{group}/{object_type}",
                .headers = headers),
                query_params)
      )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get properties of biotypes by name
#'
#' Retrieves the properties of biotypes with a given name. Optionally, the object type (gene or transcript) can be provided for filtering.
#'
#' @param name A string representing the biotype name (e.g., "protein_coding").
#' @param object_type (Optional) A string specifying the object type ("gene" or "transcript").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#'
#' @return A list of parsed JSON responses containing the properties of biotypes with the given name.
#'
#' See more about the implemented endpoint [get_biotypes_by_name()]
#' on the following [GET info/biotypes/name/:name/:object_type](https://rest.ensembl.org/documentation/info/biotypes_name)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_biotypes_by_name("protein_coding")
#' get_biotypes_by_name("protein_coding", object_type = "gene")
#' get_biotypes_by_name("protein_coding", object_type = "gene", callback = "randomlygeneratedname")
get_biotypes_by_name <- function(name, object_type = "",
                                 callback = "randomlygeneratedname") {
  if (missing(name)) {
    stop("'name' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(name)) query_params$name <- name
    if (!is.null(object_type)) query_params$object_type <- object_type
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <-
      do.call(get,
              c(list(
                res = "/info/compara/species_sets/{name}/{object_type}",
                .headers = headers),
                query_params)
    )

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get list of compara methods
#'
#' Retrieves a list of all compara analyses available (an analysis defines the type
#' of comparative data). Optional filtering by class or compara database can be applied.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine
#' for JSONP responses.
#' @param class (Optional) A string specifying the class of the method to query for.
#' Regular expression patterns are supported (e.g., "GenomicAlign").
#' @param compara (Optional) A string representing the name of the compara database
#' to use (e.g., "vertebrates").
#'
#' @return A list of parsed JSON responses containing all available compara methods.
#'
#' See more about the implemented endpoint [get_compara_methods()]
#' on the following [GET info/compara/methods](https://rest.ensembl.org/documentation/info/compara_methods)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_compara_methods()
#' get_compara_methods(class = "GenomicAlign")
#' get_compara_methods(compara = "vertebrates", class = "GenomicAlign")
get_compara_methods <- function(callback = "randomlygeneratedname",
                                class = NULL, compara = NULL) {

  if (!is.null(callback)) {
    url <- "/info/compara/methods"
    query_params <- list()
    if (!is.null(class)) query_params$class <- class
    if (!is.null(compara)) query_params$compara <- compara
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get, c(list(res = url, .headers = headers), query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  return(response)
}

#' Get collections of species analysed with a specified compara method
#'
#' Retrieves a list of all collections of species analysed with the specified compara method.
#' The compara method must be one of the methods returned by the `/info/compara/methods` endpoint.
#'
#' @param method A string representing the compara method to filter by (e.g., "EPO").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param compara (Optional) A string representing the name of the compara database to use (e.g., "vertebrates").
#'
#' @return A list of parsed JSON responses containing all collections of species analysed with the specified compara method.
#'
#' See more about the implemented endpoint [get_compara_species_sets()]
#' on the following [GET info/compara/species_sets/:method](https://rest.ensembl.org/documentation/info/compara_species_sets)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_compara_species_sets("EPO")
#' get_compara_species_sets("EPO", compara = "vertebrates")
get_compara_species_sets <- function(method, callback = "randomlygeneratedname",
                                     compara = NULL) {
  if (missing(method)) {
    stop("'method' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(method)) query_params$method <- method
    if (!is.null(compara)) query_params$compara <- compara
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/compara/species_sets/{method}",
                          .headers = headers),
                          query_params)
                        )

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }
  response
}

#' Get a list of all available comparative genomics databases and their data release
#'
#' Retrieves a list of all available comparative genomics databases and their data release.
#' This endpoint is deprecated, and users are advised to use the `/info/genomes/division` endpoint instead.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine
#' for JSONP responses.
#'
#' @return A list of parsed JSON responses containing all available comparative genomics
#' databases and their data release.
#'
#' See more about the implemented endpoint [get_comparas()]
#' on the following [GET info/comparas](https://rest.ensembl.org/documentation/info/comparas)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_comparas()
get_comparas <- function(callback = "randomlygeneratedname") {
  if (!is.null(callback)) {
    response <- get(
      res = "/info/comparas",
      callback = callback,
      .headers = req_headers(content_type = "application/json")
    )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get a list of available data releases on the Ensembl REST server
#'
#' Retrieves a list of the data releases available on the Ensembl REST server. It may return more than one release if the server has an unfrequent, non-standard configuration.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#'
#' @return A list of parsed JSON responses containing the available data releases on the Ensembl REST server.
#'
#' See more about the implemented endpoint [get_data()]
#' on the following [GET info/data](https://rest.ensembl.org/documentation/info/data)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_data()
get_data <- function(callback = "randomlygeneratedname") {
  if (!is.null(callback)) {
    response <- get(
      res = "/info/data",
      callback = callback,
      .headers = req_headers(content_type = "application/json")
    )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get the Ensembl Genomes version of the databases backing the service
#'
#' Retrieves the Ensembl Genomes version of the databases supporting the current service.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#'
#' @return A parsed JSON response containing the Ensembl Genomes version of the databases.
#'
#' See more about the implemented endpoint [get_eg_version()]
#' on the following [GET info/eg_version](https://rest.ensembl.org/documentation/info/eg_version)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_eg_version()
get_eg_version <- function(callback = "randomlygeneratedname") {
  if (!is.null(callback)) {
    response <- get(
      res = "/info/eg_version",
      callback = callback,
      .headers = req_headers(content_type = "application/json")
    )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get external databases for a species
#'
#' Retrieves a list of all available external sources for a given species.
#'
#' @param species A string representing the species name or alias (e.g., "homo_sapiens").
#' @param callback (Optional) A string representing the name of the callback subroutine
#' for JSONP responses.
#' @param feature (Optional) A string representing the feature to filter external DB entries
#' (e.g., "dna_align_feature", "protein_align_feature", "unmapped_object", "xref", "seq_region_synonym").
#' @param filter (Optional) A string to restrict external DB searches to a single source
#' or pattern (e.g., "HGNC", "GO%").
#'
#' @return A parsed JSON response containing a list of external sources associated
#' with the given species.
#'
#' See more about the implemented endpoint [get_external_dbs()]
#' on the following [GET info/external_dbs/:species](https://rest.ensembl.org/documentation/info/external_dbs)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_external_dbs("homo_sapiens")
#' get_external_dbs("homo_sapiens", feature = "xref", filter = "HGNC")
get_external_dbs <- function(species, callback = "randomlygeneratedname",
                             feature = NULL, filter = NULL) {
  if (missing(species)) {
    stop("'species' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(species)) query_params$species <- species
    if (!is.null(feature)) query_params$feature <- feature
    if (!is.null(filter)) query_params$filter <- filter
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/external_dbs/{species}",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get list of all Ensembl divisions
#'
#' Retrieves a list of all available Ensembl divisions for which information is accessible.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#'
#' @return A parsed JSON response containing the list of Ensembl divisions.
#'
#' See more about the implemented endpoint [._get_divisions()]
#' on the following [GET info/divisions](https://rest.ensembl.org/documentation/info/info_divisions)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' ._get_divisions()
#' ._get_divisions(callback = "randomlygeneratedname")
._get_divisions <- function(callback = "randomlygeneratedname") {

  if (!is.null(callback)) {
    response <- get(
      res = "/info/divisions",
      callback = callback,
      .headers = req_headers(content_type = "application/json")
    )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get genome information
#'
#' Retrieves detailed information about a given genome based on its production name.
#'
#' @param name (Required) A string representing the production name of the genome
#' (e.g., "arabidopsis_thaliana").
#' @param callback (Optional) A string representing the name of the callback subroutine
#' for JSONP responses.
#' @param expand (Optional) A boolean value (0 or 1). If set to 1, expands
#' the information to include details of sequences (can be very large).
#' Default is NULL.
#'
#' @return A parsed JSON response containing information about the specified genome.
#'
#' See more about the implemented endpoint [get_genome_info()]
#' on the following [GET info/genomes/:genome_name](https://rest.ensembl.org/documentation/info/info_genome)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genome_info(name = "arabidopsis_thaliana")
#' get_genome_info(name = "arabidopsis_thaliana", expand = 1)
#' get_genome_info(name = "arabidopsis_thaliana", callback = "randomlygeneratedname")
get_genome_info <- function(name, callback = "randomlygeneratedname",
                            expand = NULL) {
  if (missing(name)) {
    stop("The 'name' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(name)) query_params$name <- name
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(expand)) query_params$expand <- expand

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/genomes/{name}",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get genome information by INSDC accession
#'
#' Retrieves detailed information about genomes containing a specified INSDC accession.
#'
#' @param accession (Required) A string representing the INSDC sequence accession (optionally versioned), e.g., "U00096".
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param expand (Optional) A boolean value (0 or 1). If set to 1, expands the information to include details of sequences (can be very large).
#'
#' @return A parsed JSON response containing information about genomes with the specified INSDC accession.
#'
#' See more about the implemented endpoint [get_genome_info_by_accession()]
#' on the following [GET info/genomes/accession/:accession](https://rest.ensembl.org/documentation/info/info_genomes_accession)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genome_info_by_accession(accession = "U00096")
#' get_genome_info_by_accession(accession = "U00096", expand = 1)
#' get_genome_info_by_accession(accession = "U00096", callback = "randomlygeneratedname")
get_genome_info_by_accession <- function(accession, callback = "randomlygeneratedname",
                                         expand = NULL) {
  if (missing(accession)) {
    stop("The 'accession' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(accession)) query_params$accession <- accession
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(expand)) query_params$expand <- expand

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/genomes/accession/{accession}",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get genome information by assembly ID
#'
#' Retrieves information about a genome associated with a specified assembly ID.
#'
#' @param assembly_id (Required) A string representing the INSDC assembly ID (optionally versioned, e.g., "GCA_902167145.1").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param expand (Optional) A boolean value (0 or 1). If set to 1, expands the information
#' to include details of sequences (can be very large).
#'
#' @return A parsed JSON response containing information about the genome associated
#' with the specified assembly ID.
#'
#' See more about the implemented endpoint [get_genomes_by_assembly()]
#' on the following [GET info/genomes/assembly/:assembly_id](https://rest.ensembl.org/documentation/info/info_genomes_assembly)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genome_info_by_assembly(assembly_id = "GCA_902167145.1")
#' get_genome_info_by_assembly(assembly_id = "GCA_902167145.1", expand = 1)
#' get_genome_info_by_assembly(assembly_id = "GCA_902167145.1", callback = "randomlygeneratedname")
get_genome_info_by_assembly <- function(assembly_id, callback = "randomlygeneratedname",
                                    expand = NULL) {
  if (missing(assembly_id)) {
    stop("The 'assembly_id' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(assembly_id)) query_params$assembly_id <- assembly_id
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(expand)) query_params$expand <- expand

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/genomes/assembly/{assembly_id}",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get genome information for a specific division
#'
#' Retrieves information about all genomes in a given division.
#' Note: The response may be very large for divisions like Ensembl Bacteria.
#'
#' @param division_name (Required) A string representing the name of the division (e.g., "EnsemblPlants").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param expand (Optional) A boolean value (0 or 1). If set to 1, expands the information to include details of sequences (can be very large).
#'
#' @return A parsed JSON response containing information about genomes in the specified division.
#'
#' See more about the implemented endpoint [get_genomes_by_division()]
#' on the following [GET info/genomes/division/:division_name](https://rest.ensembl.org/documentation/info/info_genomes_division)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genome_info_by_division(division_name = "EnsemblPlants")
#' get_genome_info_by_division(division_name = "EnsemblPlants", expand = 1)
#' get_genome_info_by_division(division_name = "EnsemblPlants", callback = "randomlygeneratedname")
get_genome_info_by_division <- function(division_name, callback = "randomlygeneratedname",
                                        expand = NULL) {
  if (missing(division_name)) {
    stop("The 'division_name' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(division_name)) query_params$division_name <- division_name
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(expand)) query_params$expand <- expand

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/genomes/division/{division_name}",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get genome information by taxonomy node
#'
#' Retrieves information about all genomes beneath a given node of the taxonomy.
#'
#' @param taxon_name (Required) A string representing the taxon name or NCBI taxonomy ID (e.g., "Homo sapiens").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param expand (Optional) A boolean value (0 or 1). If set to 1, expands the information
#' to include details of sequences (can be very large).
#'
#' @return A parsed JSON response containing information about genomes beneath the specified taxonomy node.
#'
#' See more about the implemented endpoint [get_genomes_by_taxonomy()]
#' on the following [GET info/genomes/taxonomy/:taxon_name](https://rest.ensembl.org/documentation/info/info_genomes_taxonomy)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_genome_info_by_taxonomy(taxon_name = "Homo sapiens")
#' get_genome_info_by_taxonomy(taxon_name = "Homo sapiens", expand = 1)
#' get_genome_info_by_taxonomy(taxon_name = "Homo sapiens", callback = "randomlygeneratedname")
get_genome_info_by_taxonomy <- function(taxon_name, callback = "randomlygeneratedname",
                                        expand = NULL) {
  if (missing(taxon_name)) {
    stop("The 'taxon_name' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(taxon_name)) query_params$taxon_name <- gsub(" ", "%20", taxon_name)
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(expand)) query_params$expand <- expand

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/genomes/taxonomy/{taxon_name}",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Check Service Status
#'
#' Sends a ping request to the server to check if the service is alive.
#'
#' @param callback (Optional) A string representing the name of the callback
#' subroutine for JSONP responses.
#'
#' @return A parsed JSON response indicating the status of the service.
#'
#' See more about the implemented endpoint [ping_service()]
#' on the following [GET info/ping](https://rest.ensembl.org/documentation/info/ping)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' ping_service()
#' ping_service(callback = "randomlygeneratedname")
ping_service <- function(callback = "randomlygeneratedname") {
  if (!is.null(callback)) {
    response <- get(
      res = "/info/ping",
      callback = callback,
      .headers = req_headers(content_type = "application/json")
    )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get REST API Version
#'
#' Retrieves the current version of the Ensembl REST API.
#'
#' @param callback (Optional) A string representing the name of the callback
#' subroutine for JSONP responses.
#'
#' @return A parsed JSON response containing the REST API version information.
#'
#' See more about the implemented endpoint [._get_rest_version()]
#' on the following [GET info/rest](https://rest.ensembl.org/documentation/info/rest)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' ._get_rest_version()
#' ._get_rest_version(callback = "randomlygeneratedname")
._get_rest_version <- function(callback = "randomlygeneratedname") {

  if (!is.null(callback)) {
    response <- get(
      res = "/info/rest",
      callback = callback,
      .headers = req_headers(content_type = "application/json")
    )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get Software Version
#'
#' Retrieves the current version of the Ensembl API used by the REST server.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#'
#' @return A parsed JSON response containing the Ensembl API version information.
#'
#' See more about the implemented endpoint [._get_software_version()]
#' on the following [GET info/software](https://rest.ensembl.org/documentation/info/software)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' ._get_software_version()
#' ._get_software_version(callback = "randomlygeneratedname")
._get_software_version <- function(callback = "randomlygeneratedname") {
  if (!is.null(callback)) {
    response <- get(
      res = "/info/software",
      callback = callback,
      .headers = req_headers(content_type = "application/json")
    )
  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get Species Information
#'
#' Retrieves a list of all available species, their aliases, available adaptor groups, and data release.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param division (Optional) A string to filter by Ensembl or Ensembl Genomes division (default is "EnsemblVertebrates").
#' @param hide_strain_info (Optional) A boolean flag to show/hide strain and
#' strain_collection information (default is 0, which shows strain info).
#' @param strain_collection (Optional) A string to filter by strain collection (e.g., "mouse").
#'
#' @return A parsed JSON response containing species information.
#'
#' See more about the implemented endpoint [get_species_info()]
#' on the following [GET info/species](https://rest.ensembl.org/documentation/info/species)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_species_info()
#' get_species_info(division = "EnsemblPlants")
#' get_species_info(hide_strain_info = 1)
#' get_species_info(strain_collection = "mouse")
get_species_info <- function(callback = "randomlygeneratedname",
                             division = "EnsemblVertebrates",
                             hide_strain_info = 0, strain_collection = NULL) {

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(division)) query_params$division <- division
    if (!is.null(hide_strain_info)) query_params$hide_strain_info <- hide_strain_info
    if (!is.null(strain_collection)) query_params$strain_collection <- strain_collection

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/species",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get Variation Sources for a Species
#'
#' Retrieves the variation sources used in Ensembl for a given species.
#'
#' @param species (Required) A string representing the species name or alias (e.g., "homo_sapiens").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param filter (Optional) A string to restrict the variation source searches to a single source
#'   (e.g., "dbSNP", "ClinVar", "OMIM", "UniProt", "HGMD").
#'
#' @return A parsed JSON response containing the variation sources for the specified species.
#'
#' See more about the implemented endpoint [._get_variation_sources()]
#' on the following [GET info/variation/:species](https://rest.ensembl.org/documentation/info/variation)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' ._get_variation_sources("homo_sapiens")
#' ._get_variation_sources("homo_sapiens", filter = "ClinVar")
#' ._get_variation_sources("homo_sapiens", callback = "randomlygeneratedname")
._get_variation_sources <- function(species, callback = "randomlygeneratedname",
                                   filter = NULL) {
  if (missing(species)) {
    stop("The 'species' parameter is required.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(species)) query_params$species <- species
    if (!is.null(filter)) query_params$filter <- filter

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/variation/{species}",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get Variant Consequence Types
#'
#' Retrieves a list of all variant consequence types available in Ensembl.
#'
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param rank (Optional) A boolean (0 or 1) to include consequence ranking in the response. Default is 0.
#'
#' @return A parsed JSON response containing the list of variant consequence types.
#'
#' See more about the implemented endpoint [get_consequence_types()]
#' on the following [GET info/variation/consequence_types](https://rest.ensembl.org/documentation/info/variation_consequence_types)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_consequence_types()
#' get_consequence_types(rank = 1)
#' get_consequence_types(callback = "randomCallback")
get_consequence_types <- function(callback = "randomlygeneratedname",
                                  rank = NULL) {

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(callback)) query_params$callback <- callback
    if (!is.null(rank)) query_params$species <- rank

    headers <- req_headers(content_type = "application/json")

    response <- do.call(get,
                        c(list(
                          res = "/info/variation/consequence_types",
                          .headers = headers),
                          query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get Population Individuals
#'
#' Retrieves a list of all individuals for a specified population from a species in Ensembl.
#'
#' @param population_name (Required) A string representing the name of the population (e.g., "1000GENOMES:phase_3:ASW").
#' @param species (Required) A string representing the species name or alias (e.g., "human").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#'
#' @return A parsed JSON response containing the list of individuals for the specified population.
#'
#' See more about the implemented endpoint [get_population_individuals()]
#' on the following [GET info/variation/populations/:species/:population_name](https://rest.ensembl.org/documentation/info/variation_population_name)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_population_individuals(species = "human", population_name = "1000GENOMES:phase_3:ASW")
#' get_population_individuals(species = "homo_sapiens", population_name = "1000GENOMES:phase_3:YRI")
#' get_population_individuals(
#'   species = "human",
#'   population_name = "1000GENOMES:phase_3:CEU",
#'   callback = "randomCallback"
#' )
get_population_individuals <- function(species, population_name,
                                       callback = "randomlygeneratedname") {
  if (missing(species) || missing(population_name)) {
    stop("Both 'species' and 'population_name' are required parameters.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(species)) query_params$species <- species
    if (!is.null(population_name)) query_params$population_name <- population_name
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <-
      do.call(get,
              c(list(
                res = "/info/variation/populations/{species}/{population_name}",
                .headers = headers),
                query_params)
      )

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}

#' Get Populations for a Species
#'
#' Retrieves a list of all populations for a specified species in Ensembl.
#'
#' @param species (Required) A string representing the species name or alias (e.g., "homo_sapiens").
#' @param callback (Optional) A string representing the name of the callback subroutine for JSONP responses.
#' @param filter (Optional) A string to restrict populations returned
#' (e.g., "LD" to filter populations with linkage disequilibrium data).
#'
#' @return A parsed JSON response containing the list of populations for the specified species.
#'
#' See more about the implemented endpoint [get_species_populations()]
#' on the following [GET info/variation/populations/:species](https://rest.ensembl.org/documentation/info/variation_populations)
#' from the official [Ensembl Rest API](https://rest.ensembl.org/).
#'
#' @export
#' @examples
#' get_species_populations(species = "homo_sapiens")
#' get_species_populations(species = "human", filter = "LD")
#' get_species_populations(
#'   species = "homo_sapiens",
#'   callback = "randomlygeneratedname",
#'   filter = "LD"
#' )
get_species_populations <- function(species, callback = "randomlygeneratedname",
                                    filter = NULL) {
  if (missing(species)) {
    stop("'species' is a required parameter.")
  }

  if (!is.null(callback)) {
    query_params <- list()
    if (!is.null(species)) query_params$species <- species
    if (!is.null(filter)) query_params$filter <- filter
    if (!is.null(callback)) query_params$callback <- callback

    headers <- req_headers(content_type = "application/json")

    response <-
      do.call(get,
              c(list(
                res = "/info/variation/populations/{species}",
                .headers = headers),
                query_params))

  } else {
    warning("Callback is null. Returning an empty response.")
    response <- list()
  }

  response
}


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
