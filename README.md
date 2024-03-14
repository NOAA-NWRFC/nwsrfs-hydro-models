# NWS_hydrology_models


## Overview
The NWRFS operational hydrologic modeling suite including Sacramento Soil Moisture Accounting (SAC-SMA), Snow-17, Lagk, and ConsUse. This repo contains the original code format written in FORTRAN, rather than the equivalent Java code writen for River Forecast Center Community Hydrologic Prediction System (CHPS). 

The Northwest River Forecast Center (NWRFC), has written FORTRAN 90 wrappers to interact with the original NWRFS FORTRAN code for automated parameter optimization for model calibration. Included in this repo are Python and R packages which can compile and inteact with the FORTRAN NWRFS code, for the benefit of coupling the hydrologic models with modern optimization packages.  
