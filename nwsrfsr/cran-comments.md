## Submission

* This is version 1.0.4, a bug fix release for version 1.0.3.

## Fixes in this version

* `load_example()` and `nwsrfs_run()` accepted a `forcing_adj` argument and then
  ignored it. The model chain always applied the calibrated monthly
  climatological forcing adjustments, so `forcing_adj = FALSE` returned the
  adjusted simulation. The argument is now honoured.

* `forcing_adj` also accepts a character vector naming a subset of the forcings
  to adjust, and `load_example()`, `nwsrfs_run()` and `uh()` gained a
  `return_inst` argument for period average rather than instantaneous flow.
  Both match the interface of the companion Python package.

* The `n_clmods` bookkeeping row is dropped from the parameter table carried on
  the returned object. `chanloss()` infers the module count from the parameter
  rows when that row is absent, and still uses it when present.

* No changes to the Fortran sources, the `configure` script, or the package
  dependencies.

## R CMD check results

0 errors | 0 warnings | 0 notes

* Checked with `R CMD check --as-cran` on macOS (R 4.6.1, aarch64). The only
  local finding is that the `checkbashisms` script is not installed on the
  check machine, which is a missing local tool rather than a package issue.

## Previous submissions

* Version 1.0.3 fixed the memory issues found by the additional CRAN checks
  (r-devel gcc and valgrind) on version 1.0.2: a segfault (`memory not mapped`)
  from an example that passed zero-length parameter vectors to a Fortran routine
  which read past the end of the arrays, and conditional jumps on uninitialised
  values in the Lag/K routines (`pin7`, `flag7`, `fka7`) from setup calls that
  were disabled when the legacy code was ported to a standalone wrapper. A
  GitHub CI build with valgrind was added to catch similar issues before
  submission. That version also added the `configure` script, which probes
  whether the Fortran compiler accepts `-ffp-contract=off` and generates
  `src/Makevars` from `src/Makevars.in` with the result. It was needed because a
  compiler-dependent fused multiply-add contraction caused different results on
  Linux and Mac; with contraction off the model results agree across macOS,
  Linux and Windows, on both x86-64 and arm64.
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
