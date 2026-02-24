import numpy as np
import pandas as pd
from nwsrfs_py import adjustq

def main():
    print("Initializing AdjustQ Example...")
    print(" ")

    lid = 'NRKW1'

    # 1. Access a the example data
    fews_adjustq = adjustq.AdjustQ.load_example()

    # 2. Print out AdjustQ Inputs
    print(f'~~AdjustQ Inputs~~')
    print(pd.concat([fews_adjustq.obs_daily.rename('Daily Obs (cfs)'),fews_adjustq.obs_inst.rename('Inst Obs (cfs)'),fews_adjustq.sim.rename('Sim (cfs)')],axis=1).tail())
    print(" ")

    # 3.  Print out AdjustQ Outputs
    print(f'~~AdjustQ Outputs~~')
    print(fews_adjustq.adjustq.tail())

if __name__ == "__main__":
    main()