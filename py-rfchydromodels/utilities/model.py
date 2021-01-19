import pandas as pd
import numpy as np
import sacsnowuh.main as s

class Model:

    def __init__(self,
                 forcings: list,
                 pars: pd.DataFrame,
                 obs: pd.DataFrame):

        self.pars = pars.sort_values(['name', 'zone'])
        self.obs = obs

        zones = pars['zone'].unique()
        n_zones = len(zones)
        sim_length = forcings[0].shape[0]
        self.zones = zones
        self.n_zones = n_zones
        self.sim_length = sim_length

        # Timestep in different units
        self.dt_seconds = int((forcings[0].index[1] - forcings[0].index[0]).total_seconds())
        self.dt_days = self.dt_seconds / 86400
        self.dt_hours = self.dt_seconds / (60 * 60)

        # Extract vectors as numpy arrays for speed
        self.dates = forcings[0].index
        self.precipitation = forcings[0]['map_mm'].values
        self.temperature = forcings[0]['mat_degc'].values
        self.year = forcings[0]['year'].values
        self.month = forcings[0]['month'].values
        self.day = forcings[0]['day'].values
        self.hour = forcings[0]['hour'].values

        self.peadj_m = np.full([12, n_zones], np.nan)
        self.map_adj = np.full([12, n_zones], np.nan)
        self.mat_adj = np.full([12, n_zones], np.nan)
        self.ptps_adj = np.full([12, n_zones], np.nan)
        self.pet_adj = np.full([12, n_zones], np.nan)

        for i in range(12):
            m = i + 1
            for j, z in zip(range(n_zones), zones):
                self.peadj_m[i, j] = pars[(pars['name'] == 'peadj_' + f'{m:02}') & (pars['zone'] == z)]['value']
                self.map_adj[i, j] = pars[(pars['name'] == 'map_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
                self.mat_adj[i, j] = pars[(pars['name'] == 'mat_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
                self.pet_adj[i, j] = pars[(pars['name'] == 'pet_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']
                self.ptps_adj[i, j] = pars[(pars['name'] == 'ptps_adj_' + f'{m:02}') & (pars['zone'] == z)]['value']

        self.map = np.full([sim_length, n_zones], np.nan)
        self.mat = np.full([sim_length, n_zones], np.nan)
        self.ptps = np.full([sim_length, n_zones], np.nan)

        for i in range(len(zones)):
            self.map[:, i] = forcings[i]['map_mm']
            self.mat[:, i] = forcings[i]['mat_degc']
            self.ptps[:, i] = forcings[i]['ptps']

    def update_pars(self,pars):
        self.pars = pars

    def run(self):

        p = {}
        for par in self.pars['name'].unique():
            p[par] = self.pars[self.pars['name'] == par]['value']

        # simulates all zones
        tci = s.sacsnow(self.dt_seconds, self.year, self.month, self.day, self.hour,
                        # general pars
                        p['alat'], p['elev'],
                        # sac pars
                        p['uztwm'], p['uzfwm'], p['lztwm'], p['lzfpm'], p['lzfsm'],
                        p['adimp'], p['uzk'], p['lzpk'], p['lzsk'], p['zperc'], p['rexp'], p['pctim'],
                        p['pfree'], p['riva'], p['side'], p['rserv'], p['peadj'], p['pxadj'],
                        # monthly peadj
                        self.peadj_m,
                        # snow pars
                        p['scf'], p['mfmax'], p['mfmin'], p['uadj'], p['si'], p['nmf'], p['tipm'], p['mbase'],
                        p['plwhc'], p['daygm'], p['adc_a'], p['adc_b'], p['adc_c'],
                        # forcing adjustment
                        self.map_adj, self.mat_adj, self.pet_adj, self.ptps_adj,
                        # initial conditions
                        p['init_swe'], p['init_uztwc'], p['init_uzfwc'], p['init_lztwc'],
                        p['init_lzfsc'], p['init_lzfpc'], p['init_adimc'],
                        # forcings
                        self.map, self.ptps, self.mat)

        # channel routing
        m_uh = 1000  # max UH length
        n_uh = self.sim_length + m_uh
        sim_flow_inst_cfs = np.full([self.sim_length], 0)
        for z in range(self.n_zones):
            flow_routed = s.duamel(tci[:, z], p['unit_shape'].to_numpy()[z], p['unit_scale'].to_numpy()[z],
                                   self.dt_days, n_uh, m_uh, 1, 0)

            # instantaneous routed flow weighted by zone area
            sim_flow_inst_cfs = sim_flow_inst_cfs + flow_routed[0:self.sim_length] * 1000 * 3.28084 ** 3 / \
                           self.dt_seconds * p['zone_area'].to_numpy()[z]

        next_sim = pd.DataFrame(sim_flow_inst_cfs).shift(-1).to_numpy().flatten()
        sim_flow_cfs = (sim_flow_inst_cfs + next_sim) / 2
        self.sim_flow_cfs = pd.Series(sim_flow_cfs, index=self.dates)

        return self.sim_flow_cfs




