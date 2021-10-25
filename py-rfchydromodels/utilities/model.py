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
        if not route:
           route=[pd.DataFrame({'flow_cfs':np.zeros(len(forcings[0].index))},index=forcings[0].index,)]
           uptribs_name=['None']
           n_uptribs = 0
        else:
           uptribs_name=pars.loc[pars.type=='lagk'].zone.unique()
           n_uptribs=len(uptribs_name)
        self.uptribs_name=uptribs_name
        self.n_uptribs=n_uptribs
           

        # Timestep in different units
        self.dt_seconds = int((forcings[0].index[1] - forcings[0].index[0]).total_seconds())
        self.dt_days = self.dt_seconds / 86400
        self.dt_hours = self.dt_seconds / (60 * 60)

        # Extract vectors as numpy arrays for speed
        self.dates = forcings[0].index
        ##DELETE??##
        #self.precipitation = forcings[0]['map_mm'].values
        #self.temperature = forcings[0]['mat_degc'].values
        ##DELETE??##
        self.year = forcings[0]['year'].values
        self.month = forcings[0]['month'].values
        self.day = forcings[0]['day'].values
        self.hour = forcings[0]['hour'].values

        # fa adjustemnt parameters:  scale, p_redist, std, shift
        fa = {}

        for fa_par in self.pars.loc[self.pars['type'] == 'fa', 'name']:
            fa[fa_par] = self.pars[self.pars['name'] == fa_par]['value']

        self.map_fa_pars = pd.concat([fa['map_scale'], fa['map_p_redist'],
                                      fa['map_std'], fa['map_shift']]).astype('double').to_numpy()
        self.map_fa_pars=np.asfortranarray(self.map_fa_pars)
        
        self.mat_fa_pars = pd.concat([fa['mat_scale'], fa['mat_p_redist'],
                                      fa['mat_std'], fa['mat_shift']]).astype('double').to_numpy()
        self.mat_fa_pars=np.asfortranarray(self.mat_fa_pars)
        
        self.ptps_fa_pars = pd.concat([fa['ptps_scale'], fa['ptps_p_redist'],
                                       fa['ptps_std'], fa['ptps_shift']]).astype('double').to_numpy()
        self.ptps_fa_pars=np.asfortranarray(self.ptps_fa_pars)
        
        self.pet_fa_pars = pd.concat([fa['pet_scale'], fa['pet_p_redist'],
                                      fa['pet_std'], fa['pet_shift']]).astype('double').to_numpy()
        self.pet_fa_pars=np.asfortranarray(self.pet_fa_pars)
        
        # monthly forcing adjustments
        self.peadj_m = np.full([12, n_zones], np.nan)
        self.map_fa_limits = np.full([12, 2], np.nan)
        self.mat_fa_limits = np.full([12, 2], np.nan)
        self.ptps_fa_limits = np.full([12, 2], np.nan)
        self.pet_fa_limits = np.full([12, 2], np.nan)

        for i in range(12):
            m = i + 1
            self.map_fa_limits[i, 0] = pars[(pars['name'] == 'map_lower_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            self.map_fa_limits[i, 1] = pars[(pars['name'] == 'map_upper_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            self.mat_fa_limits[i, 0] = pars[(pars['name'] == 'mat_lower_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            self.mat_fa_limits[i, 1] = pars[(pars['name'] == 'mat_upper_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            self.ptps_fa_limits[i, 0] = pars[(pars['name'] == 'ptps_lower_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            self.ptps_fa_limits[i, 1] = pars[(pars['name'] == 'ptps_upper_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            self.pet_fa_limits[i, 0] = pars[(pars['name'] == 'pet_lower_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            self.pet_fa_limits[i, 1] = pars[(pars['name'] == 'pet_upper_' + f'{m:02}')]['value'].astype('double').to_numpy()[0]
            for j, z in zip(range(n_zones), zones):
                self.peadj_m[i, j] = pars[(pars['name'] == 'peadj_' + f'{m:02}') & (pars['zone'] == z)]['value']
        
        self.map_fa_limits=np.asfortranarray(self.map_fa_limits)
        self.mat_fa_limits=np.asfortranarray(self.mat_fa_limits)
        self.ptps_fa_limits=np.asfortranarray(self.ptps_fa_limits)
        self.pet_fa_limits=np.asfortranarray(self.pet_fa_limits)
        self.peadj_m=np.asfortranarray(self.peadj_m)
        
        self.map = np.full([sim_length, n_zones], np.nan)
        self.mat = np.full([sim_length, n_zones], np.nan)
        self.ptps = np.full([sim_length, n_zones], np.nan)

        for i in range(len(zones)):
            self.map[:, i] = forcings[i]['map_mm'].astype('double').to_numpy()
            self.mat[:, i] = forcings[i]['mat_degc'].astype('double').to_numpy()
            self.ptps[:, i] = forcings[i]['ptps'].astype('double').to_numpy()
        
        self.map=np.asfortranarray(self.map)
        self.mat=np.asfortranarray(self.mat)
        self.ptps=np.asfortranarray(self.ptps)
        
        #Get upstream tributary flows
        self.uptribs = np.full([sim_length, max(1,n_uptribs)], np.nan)
        
        for i in range(len(uptribs_name)):
            self.uptribs[:, i] = route[i]['flow_cfs'].astype('double').to_numpy()
        self.uptribs=np.asfortranarray(self.uptribs)
        
        #Get parameter values
        self.p = {}
        for par in self.pars['name'].unique():
            self.p[par] = self.pars[self.pars['name'] == par].sort_values(by='zone')['value'].to_numpy()
        #Create numpy collection of sac, snow, inital conditions
        self.init = np.concatenate([[self.p['init_swe']], [self.p['init_uztwc']], [self.p['init_uzfwc']],
                                    [self.p['init_lztwc']],[self.p['init_lzfsc']], [self.p['init_lzfpc']],
                                    [self.p['init_adimc']]
                                    ],axis=0).astype('double')
        self.init=np.asfortranarray(self.init)
        
        self.sac_pars = np.concatenate([[self.p['uztwm']], [self.p['uzfwm']], [self.p['lztwm']],
                                    [self.p['lzfpm']],[self.p['lzfsm']], [self.p['adimp']],
                                    [self.p['uzk']],[self.p['lzpk']], [self.p['lzsk']],
                                    [self.p['zperc']],[self.p['rexp']], [self.p['pctim']],
                                    [self.p['pfree']],[self.p['riva']], [self.p['side']],
                                    [self.p['rserv']],[self.p['efc']]
                                    ],axis=0).astype('double')
        self.sac_pars=np.asfortranarray(self.sac_pars)
        
        self.snow_pars = np.concatenate([[self.p['scf']], [self.p['mfmax']], [self.p['mfmin']],
                            [self.p['uadj']],[self.p['si']], [self.p['nmf']],
                            [self.p['tipm']],[self.p['mbase']], [self.p['plwhc']],
                            [self.p['daygm']],[self.p['adc_a']], [self.p['adc_b']],
                            [self.p['adc_c']]
                            ],axis=0).astype('double')
        self.snow_pars=np.asfortranarray(self.snow_pars)

        #Make dummy climo input
        self.climo=np.full((12, 4), -9999)
        self.climo=np.asfortranarray(self.climo)

    def update_pars(self, pars):
        self.pars = pars

    def lagk_run(self,n=None): 
        
        if n is None:
            n=list(range(self.n_uptribs))
        elif isinstance(n, int):
            n=[n]
            
        par = self.p

        cms_to_cfs = 35.3147
        cfs_to_cms = 1/cms_to_cfs
                
        lagk_route=s.lagk(int(self.dt_hours),int(self.dt_hours),
                    par['lagtbl_a'][n], par['lagtbl_b'][n], par['lagtbl_c'][n], par['lagtbl_d'][n],
                    par['ktbl_a'][n], par['ktbl_b'][n], par['ktbl_c'][n], par['ktbl_d'][n],
                    par['lagk_lagmax'][n], par['lagk_kmax'][n], par['lagk_qmax'][n],
                    par['lagk_lagmin'][n], par['lagk_kmin'][n], par['lagk_qmin'][n],
                    par['init_co'][n], par['init_if'][n], par['init_of'][n], par['init_stor'][n],
                    self.uptribs[:,n]*cfs_to_cms)

        sim_flow_cfs = np.sum(lagk_route,axis=1)*cms_to_cfs
        
        self.lagk_flow_cfs = pd.Series(sim_flow_cfs, index=self.dates)
        
        return self.lagk_flow_cfs

    def uh_run(self,tci,n=None):
        
        if n is None:
            n=list(range(self.n_zones))
        elif isinstance(n, int):
            n=[n]
        
        p = self.p
        
        m_uh = 1000  # max UH length
        n_uh = self.sim_length + m_uh
        sim_flow_inst_cfs = np.full([self.sim_length], 0)
        
        for y, z in zip(range(len(tci[0])),n):
            
            shape=p['unit_shape'][z]
            toc=(p['unit_toc'][z]*p['unit_toc_adj'][z])/24
            scale=toc/(shape-1+np.sqrt(shape-1))
            
            flow_routed = s.duamel(tci[:, y], float(shape), float(scale),
                                   float(self.dt_days), int(n_uh), int(m_uh), int(1), int(0))

            # flow_routed units:  mm, zone_area units:  km2,  1000 is a combined conversion of km2->m2 and mm->m
            # flow routed is depth of runoff over a basin for a time step. that with area converts it to a volume
            # and dt.second (timestep in sec) is used to complete conversion of runoff to flow
            # instantaneous routed flow weighted by zone area
            sim_flow_inst_cfs = sim_flow_inst_cfs + flow_routed[0:self.sim_length] * 1000 * 3.28084 ** 3 / \
                                self.dt_seconds * p['zone_area'][z]
        
        next_sim = pd.DataFrame(sim_flow_inst_cfs).shift(-1).to_numpy().flatten()
        sim_flow_cfs = (sim_flow_inst_cfs + next_sim) / 2
        sacsnow_flow_cfs = pd.Series(sim_flow_cfs, index=self.dates)
        
        return sacsnow_flow_cfs

    def sacsnow_run(self):

        p = self.p

        # simulates all zones
        tci = s.sacsnow(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'), self.hour.astype('int'),
                        # general pars
                        p['alat'].astype('double'), p['elev'].astype('double'), p['zone_area'].astype('double'),
                        # sac pars
                        self.sac_pars,
                        # pet and precp adjustments
                        p['peadj'].astype('double'), p['pxadj'].astype('double'),self.peadj_m,
                        # snow pars
                        self.snow_pars,
                        # forcing adjustment
                        self.map_fa_pars, self.mat_fa_pars, self.pet_fa_pars, self.ptps_fa_pars,
                        # forcing adjust limits
                        self.map_fa_limits, self.mat_fa_limits, self.pet_fa_limits, self.ptps_fa_limits,
                        # initial conditions
                        self.init,
                        # climo
                        self.climo,
                        # forcings
                        self.map, self.ptps, self.mat)

        # channel routing
        self.sacsnow_flow_cfs = self.uh_run(tci)

        return self.sacsnow_flow_cfs

    def sacsnow_states_run(self):

        p = self.p

        # simulates all zones
        states = s.sacsnowstates(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'), self.hour.astype('int'),
                        # general pars
                        p['alat'].astype('double'), p['elev'].astype('double'), p['zone_area'].astype('double'),
                        # sac pars
                        self.sac_pars,
                        # pet and precp adjustments
                        p['peadj'].astype('double'), p['pxadj'].astype('double'),self.peadj_m,
                        # snow pars
                        self.snow_pars,
                        # forcing adjustment
                        self.map_fa_pars, self.mat_fa_pars, self.pet_fa_pars, self.ptps_fa_pars,
                        # forcing adjust limits
                        self.map_fa_limits, self.mat_fa_limits, self.pet_fa_limits, self.ptps_fa_limits,
                        # initial conditions
                        self.init,
                        # climo
                        self.climo,
                        # forcings
                        self.map, self.ptps, self.mat)

        state_param=['map_fa','ptps_fa','mat_fa','etd','pet','tci','aet',
                        'uztwc','uzfwc','lztwc','lzfsc','lzfpc','adimc','swe','aesc',
                        'neghs','liqw','raim','taprev','tindex','accmax','sb','sbaesc',
                        'sbws','storage','aeadj','sndpt','sntmp']
        self.sacsnow_states={}
        for count, param in  enumerate(state_param):
            self.sacsnow_states[param]=pd.DataFrame(states[count], index=self.dates,columns=self.zones)

        #Calculate streamflow for each zone
        sf_df=pd.DataFrame()
        for count, zone in enumerate(self.zones):
            tci_zone=self.sacsnow_states['tci'][zone].astype('double').to_numpy()
            tci_zone=np.expand_dims(tci_zone,axis=1)
            tci_zone=np.asfortranarray(tci_zone)
            sf_zones=self.uh_run(tci_zone,count).rename(zone)
            sf_df=pd.concat([sf_df,sf_zones],axis=1,ignore_index=True)
        sf_df.index=self.dates
        sf_df.columns=self.zones
        
        self.sacsnow_states['sf']=sf_df

        return self.sacsnow_states
        
    def run_all(self):

        if self.n_uptribs > 0:
            self.sim = self.lagk_run() + self.sacsnow_run()
        else:
            self.sim = self.sacsnow_run()

        return self.sim
            




