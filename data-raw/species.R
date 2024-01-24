# NOTE: THIS DATASET IS NOT BEING USED AT THE MOMENT.
# THE SPECIES TABLE IS TOO BIG TO BE LOADED EACH TIME THE PACKAGE LOADS.
# IN THE FUTURE WE'LL REFACTOR THIS CODE TO SAVE A FRACTION OF THE TABLE.

# Title: Ensembl species.
# Ensembl release version: 97.
# Source: Ensembl's REST API (https://rest.ensembl.org/documentation/info/species) using ensemblr::get_species()
# Last download date: 19 July 2019.
# How to run: just source this file.
# Documentation source: R/data.R

# Output: This script generates a tibble named 'species' of 12 columns:
#   - division
#   - taxon_id
#   - species_name
#   - species_display_name
#   - species_common_name
#   - release
#   - genome_assembly_name
#   - genbank_assembly_accession
#   - strain
#   - strain_collection
#   - species_aliases
#   - groups
#
# The 'species' tibble is saved to 2 files:
#   - data/species.rda
#   - R/sysdata.rda

library(ensemblr)

species <- get_species()

usethis::use_data(species, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(species, internal = TRUE, compress = "xz", overwrite = TRUE, version = 2)
# Having this dataset also exported to R/sysdata.rda so that I can use it inside
# my functions without having R CMD check triggering a Note.
# More about this here:
#  - https://stackoverflow.com/questions/48105239/using-datasets-in-an-r-package
#  - https://support.bioconductor.org/p/24756/
