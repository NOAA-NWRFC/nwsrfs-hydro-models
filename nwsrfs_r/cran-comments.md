## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission, it adresses several comments from the initial CRAN submission. 
* The previous submission only listed the R wrapper authors. We have now credited every contributor whose code is included in the package with ctb roles (Eric Anderson, George F. Smith, and Janice M. Lewis for the legacy NWSRFS SAC-SMA/SNOW-17/LAG-K Fortran code; John E. Pask and Ondřej Čertík for the MIT-licensed sorting/types modules from the fortran-utils project), and the copyright holders as cph (NOAA for USG-authored legacy code, Battelle Memorial Institute for the PNNL-authored wrapper). A file-by-file breakdown with upstream license notices is in inst/COPYRIGHTS. Separately, to eliminate the earlier LGPL / Apache-2 license mismatch we removed the Burkardt-derived LGPL code in model_source/uh_optim.f90 (unused glomin_uh2p, dgamma_burkardt, and Brent's zero_uh2p) and replaced the one routine that was actually used with a permissively licensed root finding algorithm from Jacob Williams (https://github.com/jacobwilliams/roots-fortran). A copy of the roots-fortran license is included in the source code and in inst/LICENSE_roots_fortran.md. The package is now uniformly Apache-2 plus the MIT-compatible fortran-utils files.

## Notes

* The package wraps legacy NWS Fortran hydrologic models via `.Fortran()`.
* The authorship has been clarified with roles being changed to ctb.
* Included inst/COPYRIGHT with file-by-file details about the copyright.