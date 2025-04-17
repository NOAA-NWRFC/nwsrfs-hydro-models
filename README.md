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

### R package installation

Install the R package using the following command:

```
devtools::install_github('NOAA-NWRFC/nwsrfs-hydro-models',subdir='rfchydromodels')
```   
See the documentation `?rfchydromodels` and `?sac_snow_uh` for examples. 

### Python package installation

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

## Credits and references

Please cite the following journal article if referencing this work

Walters, G., Bracken, C., et al. A comprehensive calibration framework for the Northwest River Forecast Center. Unpublished manuscript, Submitted 2025, JAWRA (Journal of the American Water Resources Association).

If you use or adapt the code in this repository, please credit this repository as the original source.

### NWSRFS references

* Burnash, Robert J. C., et al. A generalized streamflow simulation system : conceptual modeling for digital computers. , National Weather Service, 1973
* Anderson, Eric. Snow Accumulation and Ablation Model. National Oceanic and Atmospheric Administration, 2006
* Linsley, R.K., et al. Hydrology for Engineers, McGraw-Hill series in water resources and environmental engineering. McGraw-Hill, 1982
* NOAA. Consumptive Use Operation. National Oceanic and Atmospheric Administration, 2005

## Legal disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

[NOAA GitHub Policy](https://github.com/NOAAGov/Information)
 \
 \
 \
<img src="https://www.weather.gov/bundles/templating/images/header/header.png" alt="NWS-NOAA Banner">

[National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [National Weather Service](https://www.weather.gov/) | [Northwest River Forecast Center](https://www.nwrfc.noaa.gov/rfc/)
