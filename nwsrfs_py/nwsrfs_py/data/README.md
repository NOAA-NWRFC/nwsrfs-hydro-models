# NWSRFS Example Data Directory

This directory contains bundled datasets for validating `nwsrfs_py` behavior against known outputs. The layout follows NOAA-NWRFC autocalibration conventions.

The full set of sample calibration inputs for 5 basins is archived on Zenodo: [10.5281/zenodo.18829935](https://doi.org/10.5281/zenodo.18829935). See the [autocalibration repository](https://github.com/NOAA-NWRFC/nwsrfs-hydro-autocalibration) for the calibration framework.

## Available Locations

### `NRKW1` (Nooksack River at North Cedarville, WA; USGS 12210700)

* Models: SacSnow, GammaUh, Lagk
* Data source folder: `results_por_02`

### `SFLN2` (Salmon Falls Creek near San Jacinto, NV; USGS 13105000)

* Models: SacSnow, GammaUh, Chanloss, Consuse
* Data source folder: `results_por_01`

## Station File Conventions

Required:

* `pars_optimal.csv`
* `forcing_por_*.csv`
* `flow_daily_*.csv`

Optional (workflow-dependent):

* `flow_instantaneous_*.csv`
* `upflow_*.csv`

Notes:

* The package is currently validated with a 6-hour model timestep.
* `flow_instantaneous_*.csv` is required for `AdjustQ`, but may be absent in some simulation-only workflows.

## How To Use

Use helper loaders rather than hard-coding paths:

```python
from nwsrfs_py.simulation import NwsrfsRun
from nwsrfs_py.adjustq import AdjustQ

sim = NwsrfsRun.load_example("NRKW1")
adj = AdjustQ.load_example(sim=True)
```

## `Adjustq_check` Directory

Contains baseline time series used by pytest:

* `NRKW1_w_sim.csv`: baseline for `AdjustQ.load_example(sim=True)`
* `NRKW1_wout_sim.csv`: baseline for `AdjustQ.load_example(sim=False)`
