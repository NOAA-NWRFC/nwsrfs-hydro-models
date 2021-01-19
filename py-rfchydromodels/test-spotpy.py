import pandas as pd
import numpy as np
import sacsnowuh.main as s
from utilities.model import Model
from utilities.optimization import ModelWrapper
from plotnine import *

pars_1zone = pd.read_csv('basins/TLMO3-1zone/results_01/pars_optimal.csv')
forcings_1zone = [pd.read_csv('basins/TLMO3-1zone/forcing_TLMO3-1.csv')]
obs = pd.read_csv('basins/TLMO3-2zone/results_01/optimal_6h.csv')
states = pd.read_csv('basins/TLMO3-2zone/results_01/optimal_states_6h.csv')

obs.rename(columns={'sim_flow_cfs':'sim_flow_cfs_r'},inplace=True)

forcings_1zone[0].index = pd.to_datetime(forcings_1zone[0][['year', 'month', 'day', 'hour']])
obs.index = pd.to_datetime(obs[['year', 'month', 'day', 'hour']])
obs['datetime'] = obs.index

# run 1 and 2 zone models

m_1zone = Model(forcings_1zone,pars_1zone,obs)
sim_flow_cfs_1zone = m_1zone.run()

# test the objective functions
model_wrapper = ModelWrapper(forcings_1zone,pars_1zone,obs)
print(model_wrapper.rmse())
print(model_wrapper.log())
print(model_wrapper.mae())
model_wrapper.get_objectives(['rmse','log'])