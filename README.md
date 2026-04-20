# NWRFC Operational Hydrology Models

## Overview

The Northwest River Forecast Center (NWRFC) uses the National Weather Service River Forecast System (NWSRFS) to support flood forecasting, water supply operations, drought monitoring, recreation, navigation, and environmental flow analyses.

This repository contains:

* Original NWSRFS FORTRAN source code and wrappers used for modern integrations.
* An R package (`nwsrfsr`).
* A Python package (`nwsrfs_py`) built with `meson-python` and `f2py`.

The wrapped model suite includes SAC-SMA, SNOW-17, UNIT-HG, LAG-K, CHANLOSS, and CONS_USE.

## Compatibility

* Languages: R, Python, FORTRAN 77, FORTRAN 90
* Tested compiler: [gfortran](https://gcc.gnu.org/wiki/GFortran)
* Tested OS: macOS and Red Hat Linux (Windows via WSL is expected to work)
* Tested timestep: 6-hour model timestep

## Development Environment (pixi)

[pixi](https://pixi.prefix.dev/) manages all dependencies (Python, R, gfortran, meson, etc.) in a single reproducible environment from `pixi.toml`. No separate conda env or system R install needed.

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

To activate the environment in your shell (e.g., for interactive R or Python):

```bash
pixi shell
```

If you use [direnv](https://direnv.net/), the `.envrc` includes `pixi shell-hook` so the environment activates automatically when you `cd` into the repo.

## Quick Start

### R (nwsrfsr)

From R:

```r
devtools::install_github("NOAA-NWRFC/nwsrfs-hydro-models", subdir = "nwsrfs_r")
```

From shell:

```bash
git clone https://github.com/NOAA-NWRFC/nwsrfs-hydro-models.git
cd nwsrfs-hydro-models
R CMD INSTALL nwsrfs_r
```

### Python (nwsrfs_py)

Install from PyPI:

```bash
pip install nwsrfspy
python -c "import nwsrfs_py; print('Success')"
```

For repository development, use pixi (recommended):

```bash
pixi run install-py
```

Or with conda from source:

```bash
conda create -n nwsrfs_env python=3.10
conda activate nwsrfs_env
conda install -c conda-forge fortran-compiler meson ninja

git clone https://github.com/NOAA-NWRFC/nwsrfs-hydro-models.git
cd nwsrfs-hydro-models/nwsrfs_py
pip install .
python -c "import nwsrfs_py; print('Success')"
```

Examples: `nwsrfs-hydro-models/nwsrfs_py/examples`

## Package-Specific Docs

* Python package README: `nwsrfs-hydro-models/nwsrfs_py/README.md`
* R package README: `nwsrfs-hydro-models/nwsrfs_r/README.md`

## HTML Documentation

**Live URLs**: [Main Landing Page](https://NOAA-NWRFC.github.io/nwsrfs-hydro-models/)

_This repository publishes package docs to GitHub Pages with separate paths to avoid Python/R confusion:_

>* Python Sphinx docs: `/python/`
>* R pkgdown docs: `/r/`

Local preview entry points:

* Python HTML: `nwsrfs_py/docs/build/html/index.html`
* R HTML: `nwsrfs_r/docs/reference/index.html`
* Combined landing page: `pages/index.html`

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
