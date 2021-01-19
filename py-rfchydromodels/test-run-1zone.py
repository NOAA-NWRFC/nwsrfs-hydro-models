import pandas as pd
import numpy as np
import sacsnowuh.main as s
from plotnine import *

pars = pd.read_csv('TLMO3-1zone/results_01/pars_optimal.csv')
forcings = pd.read_csv('TLMO3-1zone/forcing_TLMO3-1.csv')
flow = pd.read_csv('TLMO3-1zone/results_01/optimal_6h.csv')
states = pd.read_csv('TLMO3-1zone/results_01/optimal_states_6h.csv')

flow.index = pd.to_datetime(flow[['year', 'month', 'day', 'hour']])

zones = pars['zone'].unique()
n_zones = len(zones)
sim_length = forcings.shape[0]

dt_hours = 6
dt_seconds = dt_hours * 60 * 60
dt_days = dt_hours / 24

adj_names = ['peadj_m', 'map_adj', 'mat_adj', 'ptps_adj', 'pet_adj']
adj = {}
peadj_m = np.full([12, n_zones], np.nan)
map_adj = np.full([12, n_zones], np.nan)
mat_adj = np.full([12, n_zones], np.nan)
ptps_adj = np.full([12, n_zones], np.nan)
pet_adj = np.full([12, n_zones], np.nan)

for i in range(12):
    m = i + 1
    for j, z in zip(range(n_zones), zones):
        peadj_m[i, j] = pars[(pars['name'] == 'peadj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        map_adj[i, j] = pars[(pars['name'] == 'map_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        mat_adj[i, j] = pars[(pars['name'] == 'mat_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        pet_adj[i, j] = pars[(pars['name'] == 'pet_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        ptps_adj[i, j] = pars[(pars['name'] == 'ptps_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']

p = {}
for par in pars['name'].unique():
    p[par] = pars[pars['name'] == par]['value']

tci = s.sacsnow(dt_seconds, forcings['year'], forcings['month'], forcings['day'], forcings['hour'],
                # general pars
                p['alat'], p['elev'],
                # sac pars
                p['uztwm'], p['uzfwm'], p['lztwm'], p['lzfpm'], p['lzfsm'],
                p['adimp'], p['uzk'], p['lzpk'], p['lzsk'], p['zperc'], p['rexp'], p['pctim'],
                p['pfree'], p['riva'], p['side'], p['rserv'], p['peadj'], p['pxadj'],
                # monthly peadj
                peadj_m,
                # snow pars
                p['scf'], p['mfmax'], p['mfmin'], p['uadj'], p['si'], p['nmf'], p['tipm'], p['mbase'],
                p['plwhc'], p['daygm'], p['adc_a'], p['adc_b'], p['adc_c'],
                # forcing adjustment
                map_adj, mat_adj, pet_adj, ptps_adj,
                # initial conditions
                p['init_swe'], p['init_uztwc'], p['init_uzfwc'], p['init_lztwc'],
                p['init_lzfsc'], p['init_lzfpc'], p['init_adimc'],
                # forcings
                forcings['map_mm'], forcings['ptps'], forcings['mat_degc'])

# channel routing
m_uh = 1000 # max UH length
n_uh = sim_length + m_uh
flow_routed = s.duamel(tci, p['unit_shape'], p['unit_scale'], dt_days, n_uh, m_uh, 1, 0)

# instantaneous routed flow
sim_flow_cfs = flow_routed[0:sim_length] * 1000 * 3.28084 ** 3 / dt_seconds * p['zone_area'].to_numpy()[0]

flow.rename(columns={'sim_flow_cfs':'sim_flow_cfs_r'},inplace=True)
flow['sim_flow_cfs_inst'] = sim_flow_cfs
# convert to ave flow
flow['next_sim'] = flow['sim_flow_cfs_inst'].shift(periods=-1)
flow['sim_flow_cfs_p'] = (flow['sim_flow_cfs_inst']+flow['next_sim'])/2

tci_df = pd.DataFrame({'datetime': flow.index,
                       'tci_p': tci.flatten(),
                       'tci_r': states['tci_1']})
# fig = tci2.plot('tci_p','tci_r',kind='scatter')
(ggplot(tci_df) +
    geom_line(aes(x='datetime', y='tci_r'), color='darkgreen') +
    geom_line(aes(x='datetime', y='tci_p'), color='steelblue') +
    theme_bw())

(ggplot(tci_df) +
    geom_point(aes(x='tci_p', y='tci_r'), color='green') +
    geom_abline(aes(slope=1, intercept=0)) +
    theme_bw())

# x.plot('datetime',['sim_flow_cfs_r','sim_flow_cfs_p'])
# x.plot('sim_flow_cfs_p','sim_flow_cfs_r',kind='scatter')
(ggplot(flow) +
    geom_line(aes(x='datetime', y='sim_flow_cfs_r'), color='darkgreen') +
    geom_line(aes(x='datetime', y='sim_flow_cfs_p'), color='steelblue') +
    theme_bw())

p = (ggplot(flow) +
    geom_point(aes(x='sim_flow_cfs_p', y='sim_flow_cfs_r'), color='green') +
    geom_abline(aes(slope=1, intercept=0)) +
    theme_bw())
