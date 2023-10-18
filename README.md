
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ensemblr <img src='man/figures/logo.png' align="right" height="138.5" />

[![CRAN
status](https://www.r-pkg.org/badges/version/ensemblr)](https://CRAN.R-project.org/package=ensemblr)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of `{ensemblr}` is to provide an R client to the [Ensembl REST
API](https://rest.ensembl.org/).

Please note that this package is still in its infancy and hence only a
small fraction of the resources exposed by the Ensembl REST API are
retrievable via `{ensemblr}`. You can check the functionality covered so
far in [Ensembl REST API
Coverage](https://maialab.org/ensemblr/articles/api_coverage.html).

## Installation

You can install the current, **very** experimental version of
`{ensemblr}` with:

``` r
# install.packages("remotes")
remotes::install_github("ramiromagno/ensemblr")
```

## Cheatsheet

TODO

## Example

Retrieve human linkage disequilibrium information for variants within a
1 kilobase window centred on variant `'rs123'`:

``` r
library(ensemblr)
get_ld_variants_by_window('rs123', genomic_window_size = 1L)
#> # A tibble: 6 × 6
#>   species_name population              variant_id1 variant_id2 r_squared d_prime
#>   <chr>        <chr>                   <chr>       <chr>           <dbl>   <dbl>
#> 1 homo_sapiens 1000GENOMES:phase_3:CEU rs123       rs114           0.475   0.703
#> 2 homo_sapiens 1000GENOMES:phase_3:CEU rs123       rs10239961      0.255   1.00 
#> 3 homo_sapiens 1000GENOMES:phase_3:CEU rs123       rs122           0.722   1.00 
#> 4 homo_sapiens 1000GENOMES:phase_3:CEU rs123       rs115           0.721   1.00 
#> 5 homo_sapiens 1000GENOMES:phase_3:CEU rs123       rs124           0.722   1.00 
#> 6 homo_sapiens 1000GENOMES:phase_3:CEU rs123       rs12536724      0.255   1.00
```

## Contributors

Please note that the `{ensemblr}` project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.

## Logo

The `{ensemblr}` logo, `ensemblr.png`, is a derivative work of an
illustration of [“The small DNA double helix in PDB entry
309d”](https://cdn.rcsb.org/pdb101/motm/tiff/119-DesignedDNACrystal_309d.tif)
by [David S. Goodsell](https://ccsb.scripps.edu/goodsell/) and the [RCSB
PDB](https://www.rcsb.org/), used under
[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/).
`ensemblr.png` is licensed under CC-BY-4.0 by Ramiro Magno.

## Similar projects

- R package rensembl by [David Winter](http://david-winter.info/):
  <https://github.com/dwinter/rensembl>
- Python package ensembl-rest by [Andrés
  García](https://agargar.wordpress.com/):
  <https://github.com/Ad115/EnsemblRest>
- Python package ensembl by [Katsuya Noguchi](https://twitter.com/kn):
  <https://github.com/kn/ensembl>
