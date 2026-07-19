# nwsrfsr 1.0.3

* Fix a segfault (`memory not mapped`) reached by the `rsnwelev()` example on
  the CRAN gcc build. The example passed zero-length `talr`/`pxtemp` vectors to
  the Fortran routine, which reads them as `dimension(n_hrus)`, causing an
  out-of-bounds read. `rsnwelev()` now selects one parameter value per forcing
  zone and raises a clear R error when `elev`, `talr` or `pxtemp` are missing,
  and the example supplies the required parameters.
* Fix uninitialised-memory reads reported by valgrind in the Lag/K routines.
  `pin7`/`fka7` compared an uninitialised dimension string (setup calls were
  disabled in the wrapper port) and `flag7` read unwritten slots of its scratch
  array; all three are now initialised. Model output is unchanged.
* Fix two `-Wlto-type-mismatch` warnings reported by the CRAN LTO and gcc-ASAN
  builds. The SNOW-17 `/SNUP19/` common block left its first member `MFC`
  implicitly typed (INTEGER) in `aesc19` while it is REAL elsewhere, and
  `/SNCO19/` carried an extra trailing `TAPREV` member in `zero19` that the
  other units do not (in this port `TAPREV` is a subroutine argument, not a
  common member). Both are now declared consistently. Model output is unchanged.
* Declare the `umemst` dummy array as assumed-size (`IARRAY(*)`) instead of the
  legacy `IARRAY(1)`, which made a `gfortran -fcheck=all` build abort with a
  spurious out-of-bounds error on the first real write. This lets the package
  run under Fortran runtime bounds checking. Model output is unchanged.
* Disable floating-point expression contraction so simulation results agree
  across platforms. Compilers may fuse `a*b+c` into a single-rounding fused
  multiply-add, and whether they do varies by compiler version and CPU; in this
  iterated single-precision model chain the one-ulp differences are amplified
  by snow model thresholds into visibly different simulations (macOS/arm64 vs
  Linux/x86-64 differed by up to a few hundred cfs at individual flow peaks
  over a 43 year run). A new `configure` script probes the Fortran compiler
  for `-ffp-contract=off` and applies it via a generated `src/Makevars`
  (`Makevars.win` sets it directly since Rtools is always gfortran). With
  contraction off, results agree across macOS, Linux, x86-64 and arm64 to
  within libm rounding, which totals ~1e-4 mm of runoff over four decades.
  Model output changes very slightly on platforms that previously contracted.

# nwsrfsr 1.0.2

* Fix installation failure under parallel make (`make -j`) by declaring the
  missing Fortran module dependencies (`sorting.o: types.o` and
  `sac_snow.o: utilities.o`) in `src/Makevars` and `src/Makevars.win`.

# nwsrfsr 1.0.0

* Initial CRAN release
* Low-level Fortran wrappers: sac_snow(), uh(), lagk(), chanloss(), consuse(), fa_nwrfc()
* High-level orchestration: nwsrfs_run(), load_example(), update_pars()
* AdjustQ preprocessing: adjustq(), adjustq_load_example()
* Bundled example data for NRKW1 and SFLN2 stations
