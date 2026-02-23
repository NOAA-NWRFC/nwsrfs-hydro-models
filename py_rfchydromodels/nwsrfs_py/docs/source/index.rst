.. NWSRFSpy documentation master file, created by
   sphinx-quickstart on Mon Feb  9 06:23:28 2026.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to NWSRFSpy
===================

**NWSRFSpy** is a Python library that provides a high-performance interface to the National Weather Service River Forecast System (NWSRFS) hydrologic models.

It utilizes **F2PY** to wrap the original Fortran source code for models like **SAC-SMA**, **SNOW-17**, **Lag-K**, and **UNIT-HG**, enabling vectorized execution directly within Python. This library is designed to support the **NWS-NWRFC** autocalibration workflow.

Key Features
------------

* **Vectorized Execution:** Run models across multiple zones and timesteps simultaneously using NumPy arrays.
* **Pandas Integration:** Inputs and outputs are handled via Pandas DataFrames for easy analysis.
* **Core Models:**
    * **SacSnow:** Combined wrapper for SAC-SMA and SNOW-17.
    * **Gamma UNIT-HG:** Gamma Unit Hydrograph generation and routing.
    * **Lag-K:** Lag and K routing for upstream reaches.
    * **CONS_USE** Irrigation diversion adjustments.
    * **CHANLOSS** Channel loss, natural or anthropogenic, adjustments.

Installation
------------

.. code-block:: bash

   git clone https://github.com/NOAA-NWRFC/nwsrfs-hydro-models.git
   cd nwsrfs-hydro-models/py_rfchydromodels/nwsrfs_py
   pip install .

Usage Example
-------------

Here is a simple example of initializing a run using the NWRFC AutoCalibration tools:

.. code-block:: python

   from nwsrfs_py import simulation

   # Initialize a run with package example data
   model_run = simulation.NwsrfsRun.load_example('NRKW1')

   # Access the simulated streamflow
   sim_flow = model_run.sim
   print(sim_flow.head())

Documentation
-------------


.. toctree::
   :maxdepth: 2
   :caption: Contents:

   modules
   license
