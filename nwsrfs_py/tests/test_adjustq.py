import pytest
import pandas as pd, numpy as np
#import pdb; pdb.set_trace()

def test_adjustq_w_sim_logic(adjustq_w_sim):

    assert adjustq_w_sim.obs_daily is not None
    assert adjustq_w_sim.obs_inst is not None
    assert adjustq_w_sim.sim is not None

def test_adjustq_wout_sim_logic(adjustq_wout_sim):

    assert adjustq_wout_sim.obs_daily is not None
    assert adjustq_wout_sim.obs_inst is not None
    assert adjustq_wout_sim.sim is None

def test_adjustq_w_sim(adjustq_w_sim,adjustq_w_sim_baseline):
    
    pd.testing.assert_frame_equal(
        adjustq_w_sim.adjustq.to_frame(), 
        adjustq_w_sim_baseline.rename({'Inst_Streamflow_cfs':'AdjustQ_Inst'},axis=1), 
        check_names=False,
        check_freq=False
    )

    

def test_adjustq_wout_sim(adjustq_wout_sim,adjustq_wout_sim_baseline):

    pd.testing.assert_frame_equal(
        adjustq_wout_sim.adjustq.to_frame(), 
        adjustq_wout_sim_baseline.rename({'Inst_Streamflow_cfs':'AdjustQ_Inst'},axis=1), 
        check_names=False,
        check_freq=False
    )