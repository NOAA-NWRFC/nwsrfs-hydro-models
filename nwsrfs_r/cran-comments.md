## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.
* The LICENSE file contains the standard Apache License 2.0 text plus a U.S. Federal
  Government copyright disclaimer (17 U.S.C. §105), which is why it is included
  alongside the CRAN-recognized `Apache License (>= 2)` identifier.

## Notes

* The package wraps legacy NWS Fortran hydrologic models via `.Fortran()`.
* Fortran source in `src/` is shared with a companion Python package via symlink;
  the symlink is resolved at build time.
* Some examples use `\dontrun{}` because they require FA-adjusted forcing data
  that is not set up in the example code. The high-level entry points
  (`load_example()`, `fa_nwrfc()`, `fa_adj_nwrfc()`) use `\donttest{}` and
  run successfully.
