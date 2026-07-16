## Resubmission

* This is a resubmission to fix build failures on the CRAN build machines. 
* The earlier submission also addressed several comments from the initial CRAN submission. 
* The 2 local check notes were about the new submission, and a compiler flag that is not 
  used on other systems, which was added atomatically by the local build

## Notes

* The version number has been changed to 1.0.2. Version 1.0.0 was accepted but failed
  to install on the CRAN build machines because a parallel make 
  compiled a Fortran source before the module it `use`'d, so there was no `.mod`
  file available. We have declared the missing Fortran module dependencies 
  explicitly in `src/Makevars` and `src/Makevars.win` as recommended in 
  in the manual, and added a CI step that installs under a parallel 
  make so this cannot regress.

## R CMD check results

0 errors | 0 warnings | 2 notes


## Initial submission 

* Addressed several comments from the initial CRAN submission. 
* The previous submission only listed the R wrapper authors. We have now credited every contributor whose code is included in the package with ctb roles (Eric Anderson, George F. Smith, and Janice M. Lewis for the legacy NWSRFS SAC-SMA/SNOW-17/LAG-K Fortran code; John E. Pask and Ondřej Čertík for the MIT-licensed sorting/types modules from the fortran-utils project), and the copyright holders as cph (NOAA for USG-authored legacy code, Battelle Memorial Institute for the PNNL-authored wrapper). A file-by-file breakdown with upstream license notices is in inst/COPYRIGHTS. The Burkardt code licencing issue has also been fixed, some unused LGPL code was removed and we are now using Burkardt's MIT licensed f90 version of the Brent's original algorithm. All included code in the package is now Apache-2 compatible.
* The 2 local check notes were about the new submission, and a compiler flag that is not used on other systems, which was added atomatically by the local build
* Removed \dontrun from examples.
* The package wraps legacy Fortran hydrologic models From the U.S. National Weather Service (via `.Fortran()`).
* The authorship has been clarified with lagacy code author roles being changed to ctb.
* Included inst/COPYRIGHTS with detailed information regarding the copyright of each file.