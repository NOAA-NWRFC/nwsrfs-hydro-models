import numpy as np
import pandas as pd
from nwsrfs_py import simulation

def main():
    print("Initializing NWSRFS Simulation Example...")
    print(" ")

    lid = 'NRKW1'

    # 1. Access a the example data
    nwsrfs_sim = simulation.NwsrfsRun.load_example(lid)

    # 2. Print out the configuration
    print(f'~~Model Configuration~~')
    print(f'Site:  {lid}')
    print(f'Number of zones:  {nwsrfs_sim.n_zones}')
    print(f'Snow17/SAC-SMA/UNIT-HG Models:  {nwsrfs_sim.localflow_logic}')
    print(f'LAG-K Model:  {nwsrfs_sim.upflow_logic}')
    print(f'CHANLOSS Models:  {nwsrfs_sim.chanloss_logic}')
    print(f'CONS_USE Models:  {nwsrfs_sim.consuse_logic}')
    print(" ")

    # 3.  Print out unit hydrograph
    print('~~~UNIT-HG~~')
    print(nwsrfs_sim.uh)
    print(" ")

    # 4.  Print out unit hydrograph
    print('~~~Monthly Climatological Forcing Adjustments~~')
    print(nwsrfs_sim.fa_factors)
    print(" ")

    # 5. Print out simulation
    print('~~~Streamflow Simulation~~')
    print(nwsrfs_sim.sim.head()) 

if __name__ == "__main__":
    main()