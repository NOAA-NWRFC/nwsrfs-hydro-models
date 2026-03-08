## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.
* The LICENSE file contains a U.S. Federal Government copyright disclaimer 
  (17 U.S.C. §105), which is why it is included alongside the CRAN-recognized 
  `Apache License (>= 2)` identifier.

## Notes

* The package wraps legacy NWS Fortran hydrologic models via `.Fortran()`.
* Fortran source in `src/` is shared with a companion Python package via symlink;
  the symlink is resolved at build time.
* Some examples use `\dontrun{}` because they require data
  that is not set up in the example code. The high-level entry points
  (`load_example()`, `fa_nwrfc()`, `fa_adj_nwrfc()`) use `\donttest{}` and
  run successfully.
* Package includes both Makevars and Makevars.win. The Windows variant omits -fPIC (not needed on Windows). Win-builder R-devel, R-release, and R-oldrelease all build and pass tests successfully with 0 errors
  and 0 warnings.

## Win-builder results

Tested on win-builder with 0 errors, 0 warnings, 2 NOTEs (new submission + license)
across all platforms:

  - R-release: https://win-builder.r-project.org/T6Egg9Kbaqu8/
  - R-oldrelease: https://win-builder.r-project.org/SNPBLmbsk2xE/
  - R-devel: https://win-builder.r-project.org/J2KmSRwBV8Ut/
  - R-devel (with Makevars.win): https://win-builder.r-project.org/1Z5VozPj1YRl/
  - https://win-builder.r-project.org/qM24yJf454P9/
