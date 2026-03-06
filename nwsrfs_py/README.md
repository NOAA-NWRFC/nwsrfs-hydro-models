# NWSRFSpy

`nwsrfs_py` is a Python interface to NWSRFS hydrologic models using `f2py` wrappers around FORTRAN implementations.

It is designed to support [NWRFC autocalibration](https://github.com/NOAA-NWRFC/nwsrfs-hydro-autocalibration) workflows and provides classes for simulation and AdjustQ operations. Sample calibration data is archived on [Zenodo](https://doi.org/10.5281/zenodo.18829935).

## Install (PyPI)

```bash
pip install nwsrfspy
```

## Quick Start

```python
from nwsrfs_py import simulation

# Load packaged example data
model_run = simulation.NwsrfsRun.load_example("NRKW1")

# Simulated streamflow series
sim_flow = model_run.sim
print(sim_flow.head())
```

## Included Model Components

* SAC-SMA + SNOW-17 (`SacSnow`)
* UNIT-HG (`GammaUh`)
* LAG-K (`Lagk`)
* CHANLOSS (`Chanloss`)
* CONS_USE (`Consuse`)

## Documentation

Python docs: [https://NOAA-NWRFC.github.io/nwsrfs-hydro-models/python/](https://NOAA-NWRFC.github.io/nwsrfs-hydro-models/python/)

## Build From Source (Developers)

Source builds require a Fortran toolchain plus build tools.

Requirements:

* Python 3.10+
* `numpy`, `pandas`, `scipy`
* `gfortran`
* `meson`, `meson-python`, `ninja`

### Using pixi (recommended)

From repository root:

```bash
pixi run install-py
```

### Using conda

```bash
conda create -n nwsrfs_env python=3.10
conda activate nwsrfs_env
conda install -c conda-forge fortran-compiler meson meson-python ninja
```

Install from source:

```bash
git clone https://github.com/NOAA-NWRFC/nwsrfs-hydro-models.git
cd nwsrfs-hydro-models/nwsrfs_py
pip install .
python -c "import nwsrfs_py; print('Success!')"
```

Editable install:

```bash
pip install -e . --no-build-isolation -v
```

For runnable scripts in the source tree, see `examples/`.

## Citation

If you use this package, please cite:

Walters, G., Bracken, C., et al., "A comprehensive calibration framework for the Northwest River Forecast Center." Journal of the American Water Resources Association (JAWRA), accepted for publication in 2026. [Preprint](https://eartharxiv.org/repository/view/8993/)
