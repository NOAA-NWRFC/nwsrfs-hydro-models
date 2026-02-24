# NWSRFSpy Examples

Run commands from this directory:

```bash
cd nwsrfs-hydro-models/nwsrfs_py/examples
```

## `run_simulation.py`

Creates a `nwsrfs_py.simulation.NwsrfsRun` object and prints:

* model configuration
* unit hydrograph output
* climatological forcing adjustment factors
* simulation output preview

```bash
python run_simulation.py
```

## `run_adjustq.py`

Creates a `nwsrfs_py.adjustq.AdjustQ` object and prints:

* input time series summary
* AdjustQ output preview

```bash
python run_adjustq.py
```

## `run_nwsrfs_wrapper.py`

Creates low-level wrapper classes (`SacSnowPars`, `SacSnow`) and prints:

* total channel inflow (`tci`) preview

```bash
python run_nwsrfs_wrapper.py
```
