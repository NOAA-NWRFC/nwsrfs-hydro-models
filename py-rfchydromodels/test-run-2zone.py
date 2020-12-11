import pandas as pd
import numpy as np
import sacsnowuh.main as s

pars = pd.read_csv('TLMO3-2zone/results_01/pars_optimal.csv')
forcings = [pd.read_csv('TLMO3-2zone/forcing_TLMO3-1.csv'),
            pd.read_csv('TLMO3-2zone/forcing_TLMO3-2.csv')]

zones = pars['zone'].unique()
n_zones = len(zones)
sim_length = forcings[0].shape[0]

dt_hours = 6
dt_seconds = dt_hours * 60 * 60
dt_days = dt_hours / 24

peadj_m = np.full([12,n_zones],np.nan)
map_adj = np.full([12,n_zones],np.nan)
mat_adj = np.full([12,n_zones],np.nan)
ptps_adj = np.full([12,n_zones],np.nan)
pet_adj = np.full([12,n_zones],np.nan)

for i in range(12):
    m = i + 1
    for j,z in zip(range(n_zones),zones):
        peadj_m[i,j] = pars[(pars['name'] == 'peadj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        map_adj[i,j] = pars[(pars['name'] == 'map_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        mat_adj[i,j] = pars[(pars['name'] == 'mat_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        pet_adj[i,j] = pars[(pars['name'] == 'pet_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        ptps_adj[i,j] = pars[(pars['name'] == 'ptps_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']

map = np.full([sim_length,n_zones],np.nan)
mat = np.full([sim_length,n_zones],np.nan)
ptps = np.full([sim_length,n_zones],np.nan)

for i in range(len(zones)):
    map[:,i] = forcings[i]['map_mm']
    mat[:,i] = forcings[i]['mat_degc']
    ptps[:,i] = forcings[i]['ptps']

p = {}
for par in pars['name'].unique():
    p[par] = pars[pars['name'] == par]['value']

tci = s.sacsnow(dt_seconds, forcings[0]['year'], forcings[0]['month'], forcings[0]['day'], forcings[0]['hour'],
                # sac pars
                p['alat'], p['elev'], p['uztwm'], p['uzfwm'], p['lztwm'], p['lzfpm'], p['lzfsm'],
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
                map, mat, ptps)

# channel routing
m_uh = 1000
n_uh = sim_length + m_uh
flow_cfs = np.full([sim_length],np.nan)
for i in range(len(zones)):
    flow_routed = s.duamel(tci[:,i], p['unit_shape'].to_numpy()[i], p['unit_scale'].to_numpy()[i],
                           dt_days, n_uh, m_uh, 1, 0)

    flow_cfs = flow_cfs + flow_routed[:-m_uh] * 1000 * 3.28084**3 / dt_seconds * p['zone_area'].to_numpy()[i]


x = pd.Series(flow_cfs)
x.plot()