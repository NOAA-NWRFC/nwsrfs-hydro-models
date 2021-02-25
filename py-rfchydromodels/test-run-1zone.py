import pandas as pd
import numpy as np
import nwsrfs_models.main as s
from utilities.forcing_adjust import *
from plotnine import *

basin = 'TLMO3'

pars = pd.read_csv('basins/TLMO3-1zone/results_01/pars_optimal.csv')
forcings = pd.read_csv('basins/TLMO3-1zone/forcing_TLMO3-1.csv')
flow = pd.read_csv('basins/TLMO3-1zone/results_01/optimal_6h.csv')
states = pd.read_csv('basins/TLMO3-1zone/results_01/optimal_states_6h.csv')
fa_limits = pd.read_csv('basins/forcing-adj-limits_climo.csv')
fa_limits = fa_limits[(fa_limits['basin']==basin) &
                      (fa_limits['zone']==basin+'-1')].copy()

flow.index = pd.to_datetime(flow[['year', 'month', 'day', 'hour']])

zones = pars['zone'].unique()
zones = [z for z in zones if z != basin]
n_zones = len(zones)
sim_length = forcings.shape[0]

dt_hours = 6
dt_seconds = dt_hours * 60 * 60
dt_days = dt_hours / 24

adj_names = ['peadj_m', 'map_adj', 'mat_adj', 'ptps_adj', 'pet_adj']
adj = {}
peadj_m = np.full([12, n_zones], np.nan)

for i in range(12):
    m = i + 1
    for j, z in zip(range(n_zones), zones):
        peadj_m[i, j] = pars[(pars['name'] == 'peadj_' + f'{m:02}') & (pars['zone'] == z)]['value']

# fa adjustemnt parameters:  scale, p_redist, std, shift
fa = {}

for fa_par in pars.loc[pars['type'] == 'fa', 'name']:
    fa[fa_par] = pars[pars['name'] == fa_par]['value']

map_fa_pars = pd.concat([fa['map_scale'], fa['map_p_redist'],fa['map_std'], fa['map_shift']]).to_numpy()
mat_fa_pars = pd.concat([fa['mat_scale'], fa['mat_p_redist'], fa['mat_std'], fa['mat_shift']]).to_numpy()
ptps_fa_pars = pd.concat([fa['ptps_scale'], fa['ptps_p_redist'], fa['ptps_std'], fa['ptps_shift']]).to_numpy()
pet_fa_pars = pd.concat([fa['pet_scale'], fa['pet_p_redist'], fa['pet_std'], fa['pet_shift']]).to_numpy()

# combine limits and put in calendar year order
map_fa_limits = fa_limits[fa_limits['variable']=='map'][['lower','upper']].to_numpy()
mat_fa_limits = fa_limits[fa_limits['variable']=='mat'][['lower','upper']].to_numpy()
ptps_fa_limits = fa_limits[fa_limits['variable']=='ptps'][['lower','upper']].to_numpy()
pet_fa_limits = fa_limits[fa_limits['variable']=='pet'][['lower','upper']].to_numpy()

# PET is computed on the fly so no climo
# pet_fa_limits['aorc_pet'] = climo['pet'].to_numpy()

p = {}
for par in pars['name'].unique():
    p[par] = pars[pars['name'] == par]['value']

init = np.concatenate([[p['init_swe']], [p['init_uztwc']], [p['init_uzfwc']], [p['init_lztwc']],
                       [p['init_lzfsc']], [p['init_lzfpc']], [p['init_adimc']]])

tci = s.sacsnow(dt_seconds, forcings['year'], forcings['month'], forcings['day'], forcings['hour'],
                # general pars
                p['alat'], p['elev'], p['zone_area'],
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
                map_fa_pars, mat_fa_pars, pet_fa_pars, ptps_fa_pars,
                map_fa_limits, mat_fa_limits, pet_fa_limits, ptps_fa_limits,
                # initial conditions
                init,
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
    geom_line(aes(x='flow.index', y='sim_flow_cfs_r'), color='darkgreen') +
    geom_line(aes(x='flow.index', y='sim_flow_cfs_p'), color='steelblue') +
    theme_bw())

p = (ggplot(flow) +
    geom_point(aes(x='sim_flow_cfs_p', y='sim_flow_cfs_r'), color='green') +
    geom_abline(aes(slope=1, intercept=0)) +
    theme_bw())
