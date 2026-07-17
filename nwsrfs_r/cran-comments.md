## Resubmission

* This is version 1.0.3, a resubmission to fix the memory issues found by the
  additional CRAN checks (r-devel gcc and valgrind) on version 1.0.2.

## Fixes in this version

* The `rsnwelev()` example caused a segfault (`memory not mapped`) on the
  r-devel gcc build. The example passed zero-length parameter vectors to the
  underlying Fortran routine, which reads them as `dimension(n_hrus)`, so the
  routine read past the end of the arrays. `rsnwelev()` now selects one
  parameter value per forcing zone and stops with an informative error when a
  required parameter is missing, and the example supplies the needed
  parameters. The example now runs cleanly.

* The valgrind report flagged conditional jumps on uninitialised values in the
  Lag/K Fortran routines (`pin7`, `flag7`, `fka7`). These come from setup calls
  that were disabled when the legacy NWSRFS code was ported to a standalone
  wrapper, leaving a dimension string and a scratch work array read before they
  were set. All three are now initialised explicitly. Simulated output for the
  bundled examples is bit-for-bit unchanged.

## R CMD check results

0 errors | 0 warnings | 2 notes

* The 2 notes are the pre-existing new-submission and local-compiler-flag notes
  described below; they do not appear on the CRAN machines.

## Previous submissions

* Version 1.0.2 declared the missing Fortran module dependencies explicitly in
  `src/Makevars` and `src/Makevars.win` (as recommended in the manual) to fix a
  parallel-make (`make -j`) install failure on the CRAN build machines, and
  added a CI step that installs under a parallel make so this cannot regress.
* The initial submission (1.0.0) credited every contributor whose code is included in
  the package with ctb roles (Eric Anderson, George F. Smith, and Janice M.
  Lewis for the legacy NWSRFS SAC-SMA/SNOW-17/LAG-K Fortran code; John E. Pask
  and Ondřej Čertík for the MIT-licensed sorting/types modules from the
  fortran-utils project), and the copyright holders as cph (NOAA for
  USG-authored legacy code, Battelle Memorial Institute for the PNNL-authored
  wrapper). A file-by-file breakdown with upstream license notices is in
  inst/COPYRIGHTS. All included code in the package is Apache-2 compatible.
* The package wraps legacy Fortran hydrologic models from the U.S. National
  Weather Service (via `.Fortran()`).
