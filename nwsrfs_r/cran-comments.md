## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.

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
    * https://win-builder.r-project.org/k3lBIGh75zXU/
    * https://win-builder.r-project.org/vYYQk6b0VGRM/
    * https://win-builder.r-project.org/2sor9jJyQL5r/
