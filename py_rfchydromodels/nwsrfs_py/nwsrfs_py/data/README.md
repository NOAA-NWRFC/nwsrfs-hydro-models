# NWSRFS Example Data Directory
This directory contains bundled example datasets for validating the nwsrfs_py models. These datasets follow the NOAA-NWRFC autocalibration conventions.

## Available Locations

### **NRKW1**: Nooksack River at North Cedarville, WA (USGS 12210700).

**Models**: SacSnow, GammaUh, Lagk.

**Data Source**: NWRFC Autocalibration Results (Folder: results_por_02).


### **SFLN2**: Salmon Falls Creek NR San Jacinto NV (USGS 13105000).

**Models**: SacSnow, GammaUh, Chanloss, Consuse.

**Data Source**: NWRFC Autocalibration Results (Folder: results_por_01).

## File Structure per Station

Each station subdirectory contains the following standardized CSV files:

**forcing_por_*.csv**: Precipitation (MAP), Temperature (MAT), and Percent Snow (PTPS) for each modeled zone.

**pars_optimal.csv**: Calibrated model parameters.

**flow_daily_*.csv**: Observed daily averaged streamflow.

**flow_instantaneous_*.csv**: Observed instantaneous streamflow.

**upflow_*.csv**: Upstream reach routing inputs (if applicable).

## How to use

These files are intended to be accessed via the NwsrfsRun.load_example() and the AdjustQ.load_example() method:

```python
from nwsrfs_py.simulation import NwsrfsRun
sim = NwsrfsRun.load_example('NRKW1')
```

## Adjustq_check Directory

Contains baseline adjustq timeseries for pytest.

**NRKW1_w_sim.csv**:  used to compare against NwsrfsRun.load_example(sim=True)
**NRKW1_wout_sim.csv**:  used to compare against NwsrfsRun.load_example(sim=False)