# nwsrfsr 1.1.0

* Add multi-timestep support (`1`, `2`, `3`, `4`, `6`, `8`, `12`, and `24` hours;
  `1`--`12` hours for `fa_nwrfc()`) across `nwsrfs_run()`, `sac_snow()`, `uh()`,
  `lagk()`, `chanloss()`, and `consuse()`, including updated `Lag-K` Fortran
  routing routines (`pin7`, `pina7`, `flag7`, `fka7`, `lagk_run`) and general
  subdaily-to-daily `consuse()` alignment.
* Add ordinate unit hydrograph (`uh_0`, `uh_1`, ...) and shared basin unit
  hydrograph routing in `uh()` and `nwsrfs_run()`, plus exported
  `uh_ordinates()` with S-hydrograph and monotone cubic Hermite (PCHIP)
  timestep resampling and updated `plot_uh()` support.
* Integrate `area_elev*.csv` precipitation typing via `rsnwelev()` into
  `nwsrfs_run()` and `update_pars()` (`ptps_logic`, `area_elev_logic`,
  `area_elev`), automatically excluding `ptps` from `fa_nwrfc()` adjustments
  without requiring `ptps_lower_*` / `ptps_upper_*` limit rows when `ptps` is
  derived via `rsnwelev()`.
* Add parameter lookup tables (`uh`, `area_elev`, `peadj_table`,
  `consuse_peadj_table`, `chanloss_table`) and UH metadata (`uh_type`,
  `uh_zone_names`, `shared_uh_logic`) to `"nwsrfs_run"` objects, and strengthen
  input, timestamp, and parameter bounds validation across wrappers and
  `update_pars()` to match `nwsrfspy` 1.1.0.
* Add hydrological alignment, skill-metric, and `ggplot2` diagnostic utilities
  (`sim_to_daily()`, `flow_metrics()`, `plot.nwsrfs_run()`, `theme_nwsrfs()`,
  `plot_hydrograph()`, `plot_water_year()`, `plot_fdc()`, `plot_monthly_bias()`,
  `plot_cumulative()`, `plot_scatter()`, `plot_water_supply()`, `plot_states()`,
  `plot_runoff_regime()`, `plot_water_balance()`, `plot_forcing_adjust()`,
  `plot_uh()`, `plot_adc()`, and `plot_lagk()`).
* Add Quarto walkthrough vignette (`vignette("nwsrfsr")`) covering basin input
  and parameter setup, physical unit hydrograph and SNOW-17 depletion curves,
  climatological forcing adjustments, internal SNOW-17 and SAC-SMA state
  interpretation, daily gauge alignment and validation diagnostics, channel
  losses, and consumptive use.
* Validate area-elevation curve dimensions, monotonic area/elevation values,
  and positive temperature lapse rates (`talr > 0`) in `rsnwelev()`, matching
  the Python `RSnwElevPars` checks.
* Guard `adjustq()` against non-finite observation/simulation ratios in small
  gap interpolation (falling back to difference interpolation when simulated
  flow is zero) and preserve observed gap endpoints when `blend = 0` or a blend
  window is clipped at the series boundary.

# nwsrfsr 1.0.4

* Fix `forcing_adj` being ignored by `load_example()` and `nwsrfs_run()`. The
  argument was accepted and stored on the returned object, but the model chain
  always called `fa_nwrfc()` with the calibrated adjustment parameters, so
  `forcing_adj = FALSE` returned the adjusted simulation. Python's
  `forcing_adj=False` changes the NRKW1 simulation by about 6 percent, which is
  why R and Python disagreed on the same call.
* `forcing_adj` now also accepts a character vector naming a subset of "map",
  "mat", "ptps" and "pet", matching the Python package. `fa_nwrfc()` and
  `fa_adj_nwrfc()` gained a corresponding `adjust` argument; `dry_run = TRUE`
  is unchanged and remains shorthand for adjusting nothing.
* Add `return_inst` to `load_example()`, `nwsrfs_run()` and `uh()`, matching
  Python. With `return_inst = FALSE` the unit hydrograph returns period average
  flow, the mean of each timestep and the one after it with the final value
  carried forward, computed before the `shift_sf` shift and before Lag-K
  routing is added. Default TRUE, which is the previous behaviour.
* Drop the `n_clmods` bookkeeping row from the `pars` table carried on an
  "nwsrfs_run" object, so it matches the Python parameter table row for row.
  `chanloss()` now infers the module count from the `cl_factor_##` rows when
  that row is absent, and still honours it when present.

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
