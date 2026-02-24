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

## Credits and References

Please cite:

Walters, G., Bracken, C., et al., "A comprehensive calibration framework for the Northwest River Forecast Center." Unpublished manuscript, submitted 2025, [Preprint](https://eartharxiv.org/repository/view/8993/)

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
