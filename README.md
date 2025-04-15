# NWRFC Operational hydrology models 


## Overview
The National Weather Service River Forecasting System (NWRFS) operational hydrologic modeling suite including Sacramento Soil Moisture Accounting (SAC-SMA), Snow-17, Lagk, and ConsUse ([link](https://www.weather.gov/owp/oh_hrl_nwsrfs_users_manual_htm_xrfsdocpdf)). This repo contains the original code written in FORTRAN, rather than the equivalent Java code writen for River Forecast Center (RFC) Community Hydrologic Prediction System (CHPS). 

The Northwest River Forecast Center (NWRFC), has written FORTRAN 90 wrappers to interact with the original NWRFS FORTRAN code to perform automated parameter optimization for model calibration. Included in this repo are Python and R packages which can compile and inteact with the FORTRAN NWRFS code, for the benefit of coupling the hydrologic models with modern optimization packages for model calibration and evaluation.  

## Installation

### R package installation

You need a fortran compiler to install this package. This package has been tested with [gfortran](https://gcc.gnu.org/wiki/GFortran). See [here](https://cran.r-project.org/bin/macosx/tools/) for an easy option on MacOS.


   devtools::install_github('cameronbracken/nwrfc-hydro-models',subdir='rfchydromodels')
   
See the documentation `?rfchydromodels` and `?sac_snow_uh` for examples. 

### Python package installation


### Legal Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

[NOAA GitHub Policy](https://github.com/NOAAGov/Information)
 \
 \
 \
<img src="https://www.weather.gov/bundles/templating/images/header/header.png" alt="NWS-NOAA Banner">

[National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [National Weather Service](https://www.weather.gov/) | [Northwest River Forecast Center](https://www.nwrfc.noaa.gov/rfc/)