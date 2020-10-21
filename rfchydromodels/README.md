
<!-- README.md is generated from README.Rmd. Please edit that file -->

rfchydromodels
==============

<!-- badges: start -->
<!-- badges: end -->

Provides functions to call Fortran versions of NWS operational hydrology
models, Sacramento Soil Moisture Accounting (SAC-SMA), Snow Accumulation
and Ablation (SNOW17). Also provides an interface to the unit hydrograph
routing model. The Fortran code used in this package is considered
“legacy” and is not supported, but it should not have any significant
differences from current operational models.

Installation
------------

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("cameronbracken/NWS_hydro_models",subdir='rfchydromodels')

Example
-------

    library(rfchydromodels)
