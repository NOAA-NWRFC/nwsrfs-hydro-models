# Welcome to NWSRFSpy

**NWSRFSpy** is a Python library that provides a high-performance interface to the National Weather Service River Forecast System (NWSRFS) hydrologic models.

It utilizes **F2PY** to wrap the original Fortran source code for models like **SAC-SMA**, **SNOW-1717**, **Lag-K**, and **UNIT-HG**, enabling vectorized execution directly within Python. This library is designed to support the **NWS-NWRFC** autocalibration workflow.

## Key Features

* **Vectorized Execution:** Run models across multiple zones and timesteps simultaneously using NumPy arrays.
* **Pandas Integration:** Inputs and outputs are handled via Pandas DataFrames for easy analysis.
* **Core Models:**
    * **SacSnow:** Combined wrapper for SAC-SMA and SNOW-17.
    * **Gamma UNIT-HG:** Gamma Unit Hydrograph generation and routing.
    * **Lag-K:** Lag and K routing for upstream reaches.
    * **CONS_USE** Irrigation diversion adjustments.
    * **CHANLOSS** Channel loss, natural or anthropogenic, adjustments.

## Installation

**Supported Python Version:** 3.10+

**Package Dependencies:**  numpy, pandas, scipy

**Fortran Compiler**:  gfortran is required

**Build Tools**:  meson and ninja

It is highly recommended to install the package in a virtual environment.
```bash
conda create -n nwsrfs_env python=3.10
conda activate nwsrfs_env
conda install -c conda-forge fortran-compiler meson ninj
```

Alternatively the compiler can be installed directly on you system:

* `macOS`: `brew install gcc`
* `Linux`: `sudo apt-get install gfortran`

Installation from 
```bash
# Optional activation on virtual environment
conda activate nwsrfs_env
# Clone repository
git clone https://github.com/NOAA-NWRFC/nwsrfs-hydro-models.git
cd nwsrfs-hydro-models/py-rfchydromodels/nwsrfs_py
pip install .
# Verify build
python -c "import nwsrfs_py; print('Success!')"
```

## Usage Example

Here is a simple example of initializing a run using the NWRFC AutoCalibration tools:

```python

   from nwsrfs_py import simulation

   # Initialize a run with package example data
   model_run = simulation.NwsrfsRun.load_example('NRKW1')

   # Access the simulated streamflow
   sim_flow = model_run.sim
   print(sim_flow.head())
```

See `nwsrfs-hydro-models/py-rfchydromodels/nwsrfs_py/examples` for more example codes demonstrating how to execute the NWSRFS models.