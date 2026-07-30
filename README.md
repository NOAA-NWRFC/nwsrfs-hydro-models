# NWRFC Operational Hydrology Models

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/nwsrfsr)](https://cran.r-project.org/package=nwsrfsr)
[![PyPI version](https://img.shields.io/pypi/v/nwsrfspy.svg)](https://pypi.org/project/nwsrfspy/)
[![Test R Package](https://github.com/NOAA-NWRFC/nwsrfs-hydro-models/actions/workflows/test-r.yml/badge.svg)](https://github.com/NOAA-NWRFC/nwsrfs-hydro-models/actions/workflows/test-r.yml)
[![Test Python Models](https://github.com/NOAA-NWRFC/nwsrfs-hydro-models/actions/workflows/test-python.yml/badge.svg)](https://github.com/NOAA-NWRFC/nwsrfs-hydro-models/actions/workflows/test-python.yml)
<!-- badges: end -->

## Overview

The Northwest River Forecast Center (NWRFC) uses the National Weather Service River Forecast System (NWSRFS) to support flood forecasting, water supply operations, drought monitoring, recreation, navigation, and environmental flow analyses.

This repository contains:

* Original NWSRFS FORTRAN source code and wrappers used for modern integrations.
* An R package ([`nwsrfsr`](https://cran.r-project.org/package=nwsrfsr)).
* A Python package ([`nwsrfspy`](https://pypi.org/project/nwsrfspy/)).

The model suite includes SAC-SMA, SNOW-17, UNIT-HG, LAG-K, CHANLOSS, and CONS_USE.


## Quick Start

### R (nwsrfsr)

From [CRAN](https://cran.r-project.org/package=nwsrfsr):

```r
install.packages('nwsrfsr')
```
### Python (nwsrfspy)

Install from [PyPI](https://pypi.org/project/nwsrfspy/):

```bash
pip install nwsrfspy
```

## Package-Specific Docs

* [Python package docs](https://noaa-nwrfc.github.io/nwsrfs-hydro-models/python/)
* [R package docs](https://noaa-nwrfc.github.io/nwsrfs-hydro-models/r/)


## Development Environment (for compiling from source)

[pixi](https://pixi.prefix.dev/) manages the Python toolchain (Python, gfortran for the Python build, meson, quarto, etc.) in a reproducible environment from `pixi.toml`. The environment will be automatically activated when you run any pixi command. 

To compile the R package from source, R needs to be installed and available on the system `PATH` along with gfortran. See `dev/README.md` for the one time R setup. 

```bash
git clone https://github.com/NOAA-NWRFC/nwsrfs-hydro-models.git
cd nwsrfs-hydro-models

pixi install                  # create environment
pixi run install-py           # build & install Python package (editable)
pixi run install-r            # build & install R package
pixi run test-py              # run Python tests
pixi run test-r               # run R tests
pixi run test-all             # run both
pixi run check-r              # R CMD check (no manual)
pixi run build-r              # build R source tarball
pixi run check-r-cran         # CRAN submission check on built tarball
pixi run build-docs           # build all docs (Python + R + R README)
pixi run build-docs-py        # build Python Sphinx docs only
pixi run build-docs-r         # build R pkgdown docs only
pixi run build-docs-r-readme  # render R README.qmd to HTML
```


## Related Repositories & Data

* **[nwsrfs-hydro-autocalibration](https://github.com/NOAA-NWRFC/nwsrfs-hydro-autocalibration)** — Automated calibration framework for NWSRFS models using evolving dynamically dimensioned search (EDDS). Uses the R and Python packages from this repository.

* **Sample calibration data**: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18829935.svg)](https://doi.org/10.5281/zenodo.18829935) — Input data for 5 example basins (FSSO3, SAKW1, SFLN2, WCHW1, WGCM8) archived on Zenodo.

## Credits and References

Please cite:

Walters, G., C. Bracken, B. Gillies, et al. 2026. “A Comprehensive Calibration Framework for the Northwest River Forecast Center.” *JAWRA Journal of the American Water Resources Association* 62, no. 2: e70112. [https://doi.org/10.1111/1752-1688.70112](https://doi.org/10.1111/1752-1688.70112)

If adapting this code, please credit this repository as the original source.

### NWSRFS References

For model background, see the [NWSRFS User Manual](https://www.weather.gov/owp/oh_hrl_nwsrfs_users_manual_htm_xrfsdocpdf)

## Acknowledgment

Guidance on compiling and running NWSRFS code was informed by work from Andy Wood ([andywood@ucar.edu](mailto:andywood@ucar.edu)) and collaborators. See [NWS_hydro_models](https://github.com/NCAR/NWS_hydro_models/).

## Legal Disclaimer

This is a scientific product and does not represent official communication from NOAA or the U.S. Department of Commerce. All code is provided "as is."

See full disclaimer: [NOAA GitHub Policy](https://github.com/NOAAGov/Information)

<img src="https://www.weather.gov/bundles/templating/images/header/header.png" alt="NWS-NOAA Banner">

[National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [National Weather Service](https://www.weather.gov/) | [Northwest River Forecast Center](https://www.nwrfc.noaa.gov/rfc/)
