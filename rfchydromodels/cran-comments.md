## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes

* The package wraps legacy NWS Fortran hydrologic models via `.Fortran()`.
* Fortran source in `src/` is shared with a companion Python package via symlink;
  the symlink is resolved at build time.
