## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission, it adresses several comments from the initial CRAN submission. 
* The previous submission only listed the R wrapper authors. We have now credited every contributor whose code is included in the package with ctb roles (Eric Anderson, George F. Smith, and Janice M. Lewis for the legacy NWSRFS SAC-SMA/SNOW-17/LAG-K Fortran code; John E. Pask and Ondřej Čertík for the MIT-licensed sorting/types modules from the fortran-utils project), and the copyright holders as cph (NOAA for USG-authored legacy code, Battelle Memorial Institute for the PNNL-authored wrapper). A file-by-file breakdown with upstream license notices is in inst/COPYRIGHTS. The Burkardt code licencing issue has also been fixed, some unused LGPL code was removed and we are now using Burkardt's MIT licensed f90 version of the Brent's original algorithm. All included code in the package is now Apache-2 compatible.
* The 2 local check notes were about the new submission, and a compiler flag that is not used on other systems, which was added atomatically by the local build

## Notes

* Removed \dontrun from examples.
* The package wraps legacy Fortran hydrologic models From the U.S. National Weather Service (via `.Fortran()`).
* The authorship has been clarified with lagacy code author roles being changed to ctb.
* Included inst/COPYRIGHTS with detailed information regarding the copyright of each file.
* win_devel: https://win-builder.r-project.org/VCRt2gxIM3Y8/
* win_release: https://win-builder.r-project.org/VG6ltxi9Gh1v/