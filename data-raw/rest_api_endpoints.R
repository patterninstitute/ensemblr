# Title: Ensembl REST API Endpoints
# Source: Ensembl's REST API (https://rest.ensembl.org/)
# Last download date: 23 July 2019.
# How to run: just source this file.
# Documentation source: R/data.R

# Output: This script generates a tibble named 'rest_api_endpoints' of 4 columns:
#   - section
#   - endpoint
#   - description
#   - last_download_date: time stamp of the date this script was last run.
#
# The 'rest_api_endpoints' tibble is saved to 3 files:
#   - data-raw/rest_api_endpoints.csv
#   - data/rest_api_endpoints.rda
#   - R/sysdata.rda


library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(lubridate)
library(usethis)
library(readr)

# URL of the page of interest.
url <- 'https://rest.ensembl.org/'

# Get the html code for Ensembl REST API landing page
# and create an XML document object.
html <- xml2::read_html(url)

# From the XML document object parse the html code
# corresponding to the table. The path string
# '/html/body/div/table' was found by manually
# inspection with a browser.
nodeset <- rvest::html_nodes(x = html, xpath = '/html/body/div/table')

# Parse the html table and convert it to a data frame.
my_table <- rvest::html_table(nodeset)[[1]]

# Take the `my_table` data frame and create a grouping variable:
# 'section_id'. Then, extract the row corresponding to the header
# of each section (a group): variable 'section'. Finally remove the
# two rows per group corresponding to metadata, and select only the
# relevant columns ('section', 'endpoint', 'description') for exporting.
# NB: The need of `ungroup()` is for `select` to drop column 'section_id',
# because otherwise it will always keep it:
# https://stackoverflow.com/questions/38511743/adding-missing-grouping-variables-message-in-dplyr-in-r
my_table %>%
  dplyr::rename(endpoint = X1, description = X2) %>%
  dplyr::mutate(section_id = dplyr::lead(cumsum(endpoint == 'Resource'))) %>%
  tidyr::fill(section_id) %>%
  dplyr::group_by(section_id) %>%
  dplyr::mutate(section = dplyr::first(endpoint)) %>%
  dplyr::group_by(section_id) %>%
  dplyr::slice(-(1:2)) %>%
  dplyr::ungroup() %>%
  dplyr::select('section', 'endpoint', 'description') %>%
  dplyr::mutate(last_update_date = lubridate::date()) %>%
  dplyr::arrange(section) -> rest_api_endpoints

# Run this command to see how many endpoints per section.
# dplyr::count(rest_api_endpoints, section)

# Having this dataset also exported to R/sysdata.rda so that I can use it inside
# my functions without having R CMD check triggering a Note.
# More about this here:
#  - https://stackoverflow.com/questions/48105239/using-datasets-in-an-r-package
#  - https://support.bioconductor.org/p/24756/
readr::write_csv(rest_api_endpoints, "data-raw/rest_api_endpoints.csv")
usethis::use_data(rest_api_endpoints, compress = "xz", overwrite = TRUE, version = 2)
usethis::use_data(rest_api_endpoints, internal = TRUE, compress = "xz", overwrite = TRUE, version = 2)
