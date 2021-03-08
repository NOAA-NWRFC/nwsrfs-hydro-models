import pandas as pd
from utilities.model import Model
from utilities.problems import SacSnowUH
from plotnine import *

pars_1zone = pd.read_csv('basins/TLMO3-1zone/results_01/pars_optimal.csv')
pars_2zone = pd.read_csv('basins/TLMO3-2zone/results_01/pars_optimal.csv')

pars_2zone_2route = pd.read_csv('basins/TLMO3-2zone-2route/results_01/pars_optimal.csv')

forcings_1zone = [pd.read_csv('basins/TLMO3-1zone/forcing_TLMO3-1.csv')]
forcings_2zone = [pd.read_csv('basins/TLMO3-2zone/forcing_TLMO3-1.csv'),
                  pd.read_csv('basins/TLMO3-2zone/forcing_TLMO3-2.csv')]
                  
forcings_2zone_2route = [pd.read_csv('basins/TLMO3-2zone-2route/forcing_TLMO3-1.csv'),
                  pd.read_csv('basins/TLMO3-2zone-2route/forcing_TLMO3-2.csv')]

upflow_2zone_2route=[pd.read_csv('basins/TLMO3-2zone-2route/upflow_CONW1.csv'),
                  pd.read_csv('basins/TLMO3-2zone-2route/upflow_SAKW1.csv')]

flow = pd.read_csv('basins/TLMO3-2zone/results_01/optimal_6h.csv')
states = pd.read_csv('basins/TLMO3-2zone/results_01/optimal_states_6h.csv')

flow.rename(columns={'sim_flow_cfs':'sim_flow_cfs_r'},inplace=True)

forcings_1zone[0].index = pd.to_datetime(forcings_1zone[0][['year', 'month', 'day', 'hour']])
for f in forcings_2zone:
    f.index = pd.to_datetime(f[['year', 'month', 'day', 'hour']])
for f in forcings_2zone_2route:
    f.index = pd.to_datetime(f[['year', 'month', 'day', 'hour']])
for f in upflow_2zone_2route:
    f.index = pd.to_datetime(f[['year', 'month', 'day', 'hour']])

flow.index = pd.to_datetime(flow[['year', 'month', 'day', 'hour']])
flow['datetime'] = flow.index

# run 1 and 2 zone models

m_1zone = Model(forcings_1zone,pars_1zone,obs=flow)
sim_flow_cfs_1zone = m_1zone.run_all()

m_2zone = Model(forcings_2zone,pars_2zone,obs=flow)
sim_flow_cfs_2zone = m_2zone.run_all()
#sim_flow_cfs_2zone.resample('D').mean()

m_2zone_2route=Model(forcings_2zone_2route,pars_2zone_2route,upflow_2zone_2route,flow)
sim_flow_cfs_2zone_2routes = m_2zone_2route.run_all()

flow['sim_flow_cfs_p'] = sim_flow_cfs_2zone

p1 = (ggplot(flow) +
    geom_line(aes(x='datetime', y='sim_flow_cfs_r'), color='darkgreen') +
    geom_line(aes(x='datetime', y='sim_flow_cfs_p'), color='steelblue') +
    theme_bw())

p1 = (ggplot(flow) +
    geom_point(aes(x='sim_flow_cfs_p', y='sim_flow_cfs_r'), color='green') +
    geom_abline(aes(slope=1, intercept=0)) +
    theme_bw())

# test setting up the model wrapper
model_wrapper = SacSnowUH('TLMO3','basins/TLMO3-1zone')
