import pandas as pd
import numpy as np
# import sacsnowuh.main as s
import nwsrfs_models.main as s


class Model:

    def __init__(self,
                 forcings: list,
                 pars: pd.DataFrame,
                 route: list=None,
                 obs: pd.DataFrame=None):

        self.pars = pars.sort_values(['name', 'zone'])
        self.obs = obs
        
        zones = pars.loc[pars['zone'].str.contains('-')].zone.unique()
        n_zones = len(zones)
        sim_length = forcings[0].shape[0]
        self.zones = zones
        self.n_zones = n_zones
        self.sim_length = sim_length

        #Dummy place holder for upstream flow at headwater basins
        if route is None:
           route=[pd.DataFrame({'flow_cfs':np.zeros(len(forcings[0].index))},index=forcings[0].index,)]
           uptribs=['None']
           n_uptribs = 0
        else:
           uptribs=pars.loc[pars['zone'].str.endswith('R')].zone.unique()
           n_uptribs=len(uptribs)
        self.uptribs=uptribs
        self.n_uptribs=n_uptribs
           

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

        # fa adjustemnt parameters:  scale, p_redist, std, shift
        fa = {}

        for fa_par in self.pars.loc[self.pars['type'] == 'fa', 'name']:
            fa[fa_par] = self.pars[self.pars['name'] == fa_par]['value']

        self.map_fa_pars = pd.concat([fa['map_scale'], fa['map_p_redist'],
                                      fa['map_std'], fa['map_shift']]).to_numpy()
        self.mat_fa_pars = pd.concat([fa['mat_scale'], fa['mat_p_redist'],
                                      fa['mat_std'], fa['mat_shift']]).to_numpy()
        self.ptps_fa_pars = pd.concat([fa['ptps_scale'], fa['ptps_p_redist'],
                                       fa['ptps_std'], fa['ptps_shift']]).to_numpy()
        self.pet_fa_pars = pd.concat([fa['pet_scale'], fa['pet_p_redist'],
                                      fa['pet_std'], fa['pet_shift']]).to_numpy()

        # monthly forcing adjustments
        self.peadj_m = np.full([12, n_zones], np.nan)
        self.map_fa_limits = np.full([12, 2], np.nan)
        self.mat_fa_limits = np.full([12, 2], np.nan)
        self.ptps_fa_limits = np.full([12, 2], np.nan)
        self.pet_fa_limits = np.full([12, 2], np.nan)

        for i in range(12):
            m = i + 1
            self.map_fa_limits[i, 0] = pars[(pars['name'] == 'map_lower_' + f'{m:02}')]['value'].to_numpy()[0]
            self.map_fa_limits[i, 1] = pars[(pars['name'] == 'map_upper_' + f'{m:02}')]['value'].to_numpy()[0]
            self.mat_fa_limits[i, 0] = pars[(pars['name'] == 'mat_lower_' + f'{m:02}')]['value'].to_numpy()[0]
            self.mat_fa_limits[i, 1] = pars[(pars['name'] == 'mat_upper_' + f'{m:02}')]['value'].to_numpy()[0]
            self.ptps_fa_limits[i, 0] = pars[(pars['name'] == 'ptps_lower_' + f'{m:02}')]['value'].to_numpy()[0]
            self.ptps_fa_limits[i, 1] = pars[(pars['name'] == 'ptps_upper_' + f'{m:02}')]['value'].to_numpy()[0]
            self.pet_fa_limits[i, 0] = pars[(pars['name'] == 'pet_lower_' + f'{m:02}')]['value'].to_numpy()[0]
            self.pet_fa_limits[i, 1] = pars[(pars['name'] == 'pet_upper_' + f'{m:02}')]['value'].to_numpy()[0]
            for j, z in zip(range(n_zones), zones):
                self.peadj_m[i, j] = pars[(pars['name'] == 'peadj_' + f'{m:02}') & (pars['zone'] == z)]['value']

        self.map = np.full([sim_length, n_zones], np.nan)
        self.mat = np.full([sim_length, n_zones], np.nan)
        self.ptps = np.full([sim_length, n_zones], np.nan)

        for i in range(len(zones)):
            self.map[:, i] = forcings[i]['map_mm']
            self.mat[:, i] = forcings[i]['mat_degc']
            self.ptps[:, i] = forcings[i]['ptps']
        
        self.uptribs = np.full([sim_length, max(1,n_uptribs)], np.nan)
        
        for i in range(len(uptribs)):
            self.uptribs[:, i] = route[i]['flow_cfs']#.astype('f4').to_numpy()

        self.p = {}
        for par in self.pars['name'].unique():
            self.p[par] = self.pars[self.pars['name'] == par].sort_values(by='zone')['value'].to_numpy()
        
        self.init = np.concatenate([[self.p['init_swe']], [self.p['init_uztwc']], [self.p['init_uzfwc']],
                                    [self.p['init_lztwc']],[self.p['init_lzfsc']], [self.p['init_lzfpc']],
                                    [self.p['init_adimc']]],axis=0)

    def update_pars(self, pars):
        self.pars = pars

    def lagk_run(self,n=None): 
        
        if n is None:
            n=list(range(self.n_uptribs))
        elif isinstance(n, int):
            n=[n]
            
        par = self.p
                
        lagk_route=s.lagk(self.dt_hours,self.dt_hours,'METR',
                    par['lagtbl_a'][n],par['lagtbl_b'][n],par['lagtbl_c'][n],par['lagtbl_d'][n],
                    par['ktbl_a'][n],par['ktbl_b'][n],par['ktbl_c'][n],par['ktbl_d'][n],
                    par['lagk_lagmax'][n], par['lagk_kmax'][n],par['lagk_qmax'][n],
                    par['init_co'][n],par['init_if'][n], par['init_of'][n],par['init_stor'][n],              
                    self.uptribs[:,n]*0.0283168)
        
        sim_flow_inst_cfs=np.sum(lagk_route,axis=1)*35.3147
            
        next_sim = pd.DataFrame(sim_flow_inst_cfs).shift(-1).to_numpy().flatten()
        sim_flow_cfs = (sim_flow_inst_cfs + next_sim) / 2
        
        self.lagk_flow_cfs = pd.Series(sim_flow_cfs, index=self.dates)
        
        return self.lagk_flow_cfs

    def sacsnow_run(self,n=None):
        
        if n is None:
            n=list(range(self.n_uptribs))
        elif isinstance(n, int):
            n=[n]
            len
        
        p = self.p

        # simulates all zones
        tci = s.sacsnow(self.dt_seconds, self.year, self.month, self.day, self.hour,
                        # general pars
                        p['alat'][n], p['elev'][n], p['zone_area'][n],
                        # sac pars
                        p['uztwm'][n], p['uzfwm'][n], p['lztwm'][n], p['lzfpm'][n], p['lzfsm'][n],
                        p['adimp'][n], p['uzk'][n], p['lzpk'][n], p['lzsk'][n], p['zperc'][n], p['rexp'][n], p['pctim'][n],
                        p['pfree'][n], p['riva'][n], p['side'][n], p['rserv'][n], p['peadj'][n], p['pxadj'][n],
                        # monthly peadj
                        self.peadj_m[:,n],
                        # snow pars
                        p['scf'][n], p['mfmax'][n], p['mfmin'][n], p['uadj'][n], p['si'][n], p['nmf'][n], p['tipm'][n], p['mbase'][n],
                        p['plwhc'][n], p['daygm'][n], p['adc_a'][n], p['adc_b'][n], p['adc_c'][n],
                        # forcing adjustment
                        self.map_fa_pars, self.mat_fa_pars, self.pet_fa_pars, self.ptps_fa_pars,
                        self.map_fa_limits, self.mat_fa_limits, self.pet_fa_limits, self.ptps_fa_limits,
                        # initial conditions
                        self.init[:,n],
                        # forcings
                        self.map[:,n], self.ptps[:,n], self.mat[:,n])

        # channel routing
        m_uh = 1000  # max UH length
        n_uh = self.sim_length + m_uh
        sim_flow_inst_cfs = np.full([self.sim_length], 0)
        for y, z in zip(range(len(tci[0])),n):
            flow_routed = s.duamel(tci[:, y], p['unit_shape'][z], p['unit_scale'][z],
                                   self.dt_days, n_uh, m_uh, 1, 0)

            # flow_routed units:  mm, zone_area units:  km2,  1000 is a combined conversion of km2->m2 and mm->m
            # flow routed is depth of runoff over a basin for a time step. that with area converts it to a volume
            # and dt.second (timestep in sec) is used to complete conversion of runoff to flow
            # instantaneous routed flow weighted by zone area
            sim_flow_inst_cfs = sim_flow_inst_cfs + flow_routed[0:self.sim_length] * 1000 * 3.28084 ** 3 / \
                                self.dt_seconds * p['zone_area'][z]
        
        next_sim = pd.DataFrame(sim_flow_inst_cfs).shift(-1).to_numpy().flatten()
        sim_flow_cfs = (sim_flow_inst_cfs + next_sim) / 2
        self.sacsnow_flow_cfs = pd.Series(sim_flow_cfs, index=self.dates)

        return self.sacsnow_flow_cfs
        
    def run_all(self):
        
        self.run_all=self.lagk_run()+self.sacsnow_run()
        
        return self.run_all_flow_cfs
            




