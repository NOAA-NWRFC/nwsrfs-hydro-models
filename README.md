# NWRFC Operational Hydrology Models 

## Overview

The Northwest River Forecast Center (NWRFC) utilizes the National Weather Service River Forecasting System (NWSRFS) to provide timely information related to flooding, water supply, drought, recreation, navigation, and environmental flows. Originally developed in the late 1970s, NWSRFS remains a core component of the NWS Community Hydrologic Prediction System (CHPS). The system includes a suite of models that simulate soil moisture, snow accumulation and melt, flow routing, channel loss, and consumptive water use. For additional details on each model, see [this link](https://www.weather.gov/owp/oh_hrl_nwsrfs_users_manual_htm_xrfsdocpdf) .

To support hydrologic model calibration and development, NWRFC has created FORTRAN 90 wrappers to execute the original NWSRFS source code. This repository contains the original FORTRAN code (as opposed to the Java-based implementation used in CHPS). The wrapped suite of models includes SAC-SMA, SNOW17, Unit Hydrograph, LAGK, CHANLOSS, and CONS_USE.

Also included in this repository are Python and R packages that compile and interact with the FORTRAN 90 wrappers. These tools are intended to facilitate coupling the hydrologic models with modern optimization packages, supporting model calibration and evaluation.

**Languages:** R, Python, FORTRAN 77, and FORTRAN 90\
**Compiler:** A FORTRAN compiler is required to install this package. This package has been tested with [gfortran](https://gcc.gnu.org/wiki/GFortran). See [this page](https://cran.r-project.org/bin/macosx/tools/) for a simple installation option on macOS\
**Known OS Compatibility:** macOS and Red Hat OS\
**Time Step Compatibility:** This package and its wrappers have been tested only with a 6-hour time step. Use with other time steps may require additional configuration or validation.

## Installation

### R Package Installation

Install the R package using the following command:

```
devtools::install_github('NOAA-NWRFC/nwsrfs-hydro-models',subdir='rfchydromodels')
```   
See the documentation `?rfchydromodels` and `?sac_snow_uh` for examples. 

### Python Package Installation

**Tested Python Version:** 3.10.3\
**Package Dependencies:**  numpy, pandas, lxml\
**Dependencies:** numpy, pandas
numpy's `f2py` is used to compile the source code and FORTRAN wrappers. To compile the FORTRAN source:

```
cd py-rfchydromodels/utilities
make
```
See `nwsrfs-hydro-models/py-rfchydromodels/run_example.py` for example code demonstrating how to execute the NWSRFS models.

*Note:  An equivalent Python version of the R package is planned for a future release of this repository.*

## Credits and References

Please cite the following work when using this tool:

Walters, G., Bracken, C., et al., "A comprehensive calibration framework for the Northwest River Forecast Center." Unpublished manuscript, Submitted 2025, JAWA Journal of the American Water Resources Association

If adapting this code, please credit this repository as the original source. 

### NWSRFS References

* Burnash, Robert J. C., et al. A generalized streamflow simulation system : conceptual modeling for digital computers. , National Weather Service, 1973
* Anderson, Eric. Snow Accumulation and Ablation Model. National Oceanic and Atmospheric Administration, 2006
* Linsley, R.K., et al. Hydrology for Engineers, McGraw-Hill series in water resources and environmental engineering. McGraw-Hill, 1982
* NOAA. Consumptive Use Operation. National Oceanic and Atmospheric Administration, 2005

## Legal Disclaimer

This is a scientific product and does not represent official communication from NOAA or the U.S. Department of Commerce. All code is provided "as is."

See full disclaimer: [NOAA GitHub Policy](https://github.com/NOAAGov/Information)
 \
 \
 \
<img src="https://www.weather.gov/bundles/templating/images/header/header.png" alt="NWS-NOAA Banner">

[National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [National Weather Service](https://www.weather.gov/) | [Northwest River Forecast Center](https://www.nwrfc.noaa.gov/rfc/)
