# nwrfc_hydro_models


## Overview
The National Weather Service River Forecasting System (NWRFS) operational hydrologic modeling suite including Sacramento Soil Moisture Accounting (SAC-SMA), Snow-17, Lagk, and ConsUse ([link](https://www.weather.gov/owp/oh_hrl_nwsrfs_users_manual_htm_xrfsdocpdf)). This repo contains the original code written in FORTRAN, rather than the equivalent Java code writen for River Forecast Center (RFC) Community Hydrologic Prediction System (CHPS). 

The Northwest River Forecast Center (NWRFC), has written FORTRAN 90 wrappers to interact with the original NWRFS FORTRAN code to perform automated parameter optimization for model calibration. Included in this repo are Python and R packages which can compile and inteact with the FORTRAN NWRFS code, for the benefit of coupling the hydrologic models with modern optimization packages.  
