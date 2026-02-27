# NWSRFSpy

`nwsrfs_py` is a Python interface to NWSRFS hydrologic models using `f2py` wrappers around the FORTRAN implementation.

It is designed to support NWRFC autocalibration workflows and provides classes for simulation and AdjustQ operations.

## Included Model Components

* SAC-SMA + SNOW-17 (`SacSnow`)
* UNIT-HG (`GammaUh`)
* LAG-K (`Lagk`)
* CHANLOSS (`Chanloss`)
* CONS_USE (`Consuse`)

## Requirements

* Python 3.10+
* `numpy`, `pandas`, `scipy`
* `gfortran`
* `meson` and `ninja`

## Installation

Recommended environment setup:

```bash
conda create -n nwsrfs_env python=3.10
conda activate nwsrfs_env
conda install -c conda-forge fortran-compiler meson ninja
```

Install from source:

```bash
git clone https://github.com/NOAA-NWRFC/nwsrfs-hydro-models.git
cd nwsrfs-hydro-models/nwsrfs_py
pip install .
python -c "import nwsrfs_py; print('Success!')"
```
Note regarding installing in edit mode, use `--no-build-isolation` flag

```bash
pip install -e . --no-build-isolation -v
```

## Usage Example

```python
from nwsrfs_py import simulation

# Initialize a run with package example data
model_run = simulation.NwsrfsRun.load_example("NRKW1")

# Access simulated streamflow
sim_flow = model_run.sim
print(sim_flow.head())
```

For runnable scripts, see `examples/` in this directory.

## HTML Documentation

[**Live URL**](https://NOAA-NWRFC.github.io/nwsrfs-hydro-models/python/)

This package's Sphinx HTML docs are published on GitHub Pages under the Python-specific route:

Local preview:

```bash
# Build docs
sphinx-build -b html docs/source docs/build/html

# Open generated site
open docs/build/html/index.html
```
