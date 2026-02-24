
nwsrfsr
==============

R package wrapping NWSRFS (National Weather Service River Forecast System) Fortran
hydrologic models: SAC-SMA, SNOW-17, Unit Hydrograph, Lag-K, Chanloss, and Consuse.

Provides both low-level Fortran wrappers and a high-level orchestration layer that
reads NWRFC autocalibration directories, auto-detects model components, and runs
the full model chain. Also includes an AdjustQ module for preprocessing upstream flow.

Installation
------------

```r
# install.packages(“devtools”)
devtools::install_github(“NOAA-NWRFC/nwsrfs-hydro-models”, subdir = “nwsrfs_r”)
```

Or from a local clone:

```bash
R CMD INSTALL nwsrfs_r
```

Quick Start
-----------

### Run a bundled example

```r
library(nwsrfsr)

# Run NRKW1 (2 local zones + 3 upstream tributaries via Lag-K)
run = load_example(“NRKW1”)
print(run)
plot(run$sim, type = “l”, ylab = “Flow (cfs)”, main = “NRKW1”)

# Run SFLN2 (2 local zones + chanloss + consumptive use)
run2 = load_example(“SFLN2”)
```

### Run from an autocalibration directory

```r
run = nwsrfs_run(“/path/to/autocalb/results_por_02”)
```

### Update parameters and re-run

```r
new_pars = data.frame(
  p_name = “uztwm_NRKW1-1”,
  value = 120.0
)
run2 = update_pars(run, new_pars)
```

### AdjustQ preprocessing

```r
# With simulation
result = adjustq_load_example(sim = TRUE)

# Or manually
result = adjustq(
  daily_flow = nrkw1_daily_flow,
  inst_flow = nrkw1_inst_flow,
  sim = run$sim,
  sim_dates = run$forcings[[1]][, c(“year”, “month”, “day”, “hour”)]
)
```

Bundled Data
------------

Example data for two stations:

- **NRKW1**: 2 local zones, 3 upstream tributaries (Lag-K routing), no chanloss/consuse
- **SFLN2**: 2 local zones + CU zone, chanloss, consumptive use, no upstream flow

Data objects: `nrkw1_pars`, `nrkw1_forcing`, `nrkw1_upflow`, `nrkw1_daily_flow`,
`nrkw1_inst_flow`, `sfln2_pars`, `sfln2_forcing`, `sfln2_daily_flow`, `sfln2_inst_flow`.

Testing
-------

```r
devtools::test(“nwsrfs_r”)
```
