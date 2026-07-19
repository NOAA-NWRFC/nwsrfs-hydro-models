## Resubmission

* This is version 1.0.3, a resubmission to fix the memory issues found by the
  additional CRAN checks (r-devel gcc and valgrind) on version 1.0.2.

## Fixes in this version

* An example caused a segfault (`memory not mapped`) on the r-devel gcc build. 
  The example passed zero-length parameter vectors to the underlying Fortran 
  routine which read past the end of the arrays. This has been fixed and the 
  example now runs cleanly.

* We have added a GitHub CI build with valgrind to catch any similar issues 
  before submission in the future.

* The valgrind report flagged conditional jumps on uninitialised values in the
  Lag/K Fortran routines (`pin7`, `flag7`, `fka7`). These come from setup calls
  that were disabled when the legacy code was ported to a standalone
  wrapper for this package, all three are now initialised explicitly. 

* This version adds a `configure` script. It probes whether the Fortran
  compiler accepts `-ffp-contract=off` and generates `src/Makevars` from
  `src/Makevars.in` with the result (the flag is dropped on compilers that
  reject it). Fused multiply-add contraction varies by compiler version and
  CPU and made this iterated hydrology model produce visibly different
  simulations on different platforms; with contraction off, results agree
  across macOS, Linux, Windows, x86-64 and arm64.

## R CMD check results

0 errors | 0 warnings | 2 notes

* The 2 notes are the recent resubmission and local-compiler-flag notes
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
