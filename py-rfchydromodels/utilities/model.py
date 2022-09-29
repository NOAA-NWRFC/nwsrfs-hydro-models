import warnings
import pandas as pd
import numpy as np
from scipy.special import gamma
import utilities.model_src as s


class Model:

    def __init__(self,
                 forcings: list,
                 pars: pd.DataFrame,
                 route: list=None,
                 obs: pd.DataFrame=None):

        #This step may become unnessary, but right now the R preprocessor is very verbose with
        #CU zone name.  If that changes, this line can be removed
        pars.loc[pars.zone.str.contains('CU'),['zone']]=pars.loc[pars.zone.str.contains('CU')].zone.str.split('-').str[-1]

        self.pars = pars.sort_values(['name', 'zone'])
        self.obs = obs
        
        zones = self.pars.loc[(self.pars['zone'].str.contains('-'))|(self.pars['zone'].str.contains('_'))].zone.unique()

        n_zones = len(zones)
        sim_length = forcings[0].shape[0]
        self.zones = np.sort(zones)
        self.n_zones = n_zones
        self.sim_length = sim_length

        #Catalog CONSUSE info if present
        if self.pars.zone.str.contains('_CU').any():
            consuse_name=pd.Series(self.pars.loc[self.pars.type=='consuse'].zone.unique()).str.split('-').str[-1].values
            n_consuse=len(consuse_name)
        else:
            consuse_name=['None']
            n_consuse=0
        self.consuse_name=consuse_name
        self.n_consuse=n_consuse
        
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

        #Rename Zones for each CL Module.  But do not rename cl_type or n_clmods
        cl_logic=(pars.type=='chanloss')&((pars.name!='cl_type')&(pars.name!='n_clmods'))
        self.pars.loc[cl_logic,['zone']]=self.pars.loc[cl_logic].zone + \
                                '_CL'+ self.pars.loc[cl_logic].name.str.split('_').str[-1]
        
        #n_clmods pars row now unnecessary
        self.pars.drop(self.pars.loc[self.pars.name=='n_clmods'].index,inplace=True)

        #Remove the CL module name from the name columns
        self.pars.loc[self.pars.type=='chanloss',['name']]=self.pars.loc[self.pars.type=='chanloss'].name.str.split('_').str[:-1].str.join('_')
        #Correct the cl_type name
        self.pars.loc[self.pars.p_name.str.contains('cl_type'),['name']]='cl_type'
        
        #Catalog CHANLOSS info if present
        if self.pars.zone.str.contains('_CL').any():
            n_chanloss=len(self.pars.loc[self.pars.type=='chanloss'].zone.unique())
        else:
            n_chanloss=0
        self.n_chanloss=n_chanloss
        
        # Timestep in different units
        self.dt_seconds = int((forcings[0].index[1] - forcings[0].index[0]).total_seconds())
        self.dt_days = self.dt_seconds / 86400
        self.dt_hours = self.dt_seconds / (60 * 60)

        # Extract vectors as numpy arrays for speed
        self.dates = forcings[0].index
        self.year = forcings[0]['year'].values
        self.month = forcings[0]['month'].values
        self.day = forcings[0]['day'].values
        self.hour = forcings[0]['hour'].values

        #Calculate forcing using dedicated model class
        self.forcings=Forcings(forcings,self.pars)
        self.forcings.fa_ts(self.dt_seconds,self.dates)

        #Calculate the UH using a dedicated model class
        self.uh=UH(self.pars,self.dt_hours)
        
        #format peadj for consuse calculation 
        self.peadj_cu = np.full([12, n_consuse], np.nan)
        for i in range(12):
            m = i + 1
            for j, z in zip(range(n_consuse), consuse_name):
                self.peadj_cu[i, j] = pars[(pars['name'] == 'peadj_cu_' + f'{m:02}') & (pars['zone'] == z) &
                        (pars['type']== 'consuse')]['value'].astype('double').to_numpy()[0]
        self.peadj_cu=np.asfortranarray(self.peadj_cu)

        #Get upstream tributary flows
        self.uptribs = np.full([sim_length, max(1,n_uptribs)], np.nan)
        
        for i in range(len(uptribs_name)):
            self.uptribs[:, i] = route[i]['flow_cfs'].astype('double').to_numpy()
        self.uptribs=np.asfortranarray(self.uptribs)
        
        #Get parameter values
        self.p = {}
        for par_type in self.pars['type'].unique():
            self.p[par_type]={}
            for par in self.pars.loc[self.pars.type==par_type].name.unique():
                self.p[par_type][par] = self.pars.loc[(self.pars.type==par_type)&
                    (self.pars['name'] == par)].sort_values(by='zone')['value'].to_numpy()
        
        self.sac_pars = np.concatenate([[self.p['sac']['uztwm']], [self.p['sac']['uzfwm']], [self.p['sac']['lztwm']],
                                    [self.p['sac']['lzfpm']],[self.p['sac']['lzfsm']], [self.p['sac']['adimp']],
                                    [self.p['sac']['uzk']],[self.p['sac']['lzpk']], [self.p['sac']['lzsk']],
                                    [self.p['sac']['zperc']],[self.p['sac']['rexp']], [self.p['sac']['pctim']],
                                    [self.p['sac']['pfree']],[self.p['sac']['riva']], [self.p['sac']['side']],
                                    [self.p['sac']['rserv']],[self.p['sac']['efc']]
                                    ],axis=0).astype('double')
        self.sac_pars=np.asfortranarray(self.sac_pars)
        
        self.snow_pars = np.concatenate([[self.p['snow']['scf']], [self.p['snow']['mfmax']], [self.p['snow']['mfmin']],
                            [self.p['snow']['uadj']],[self.p['snow']['si']], [self.p['snow']['nmf']],
                            [self.p['snow']['tipm']],[self.p['snow']['mbase']], [self.p['snow']['plwhc']],
                            [self.p['snow']['daygm']],[self.p['snow']['adc_a']], [self.p['snow']['adc_b']],
                            [self.p['snow']['adc_c']]
                            ],axis=0).astype('double')
        self.snow_pars=np.asfortranarray(self.snow_pars)

    def update_pars(self, pars):
        self.pars = pars

    def lagk_run(self,n=None): 
        
        if n is None:
            n=list(range(self.n_uptribs))
        elif isinstance(n, int):
            n=[n]
            
        p = self.p['lagk']
                
        lagk=s.lagk(int(self.dt_hours),int(self.dt_hours),
                    p['lagtbl_a'][n], p['lagtbl_b'][n], p['lagtbl_c'][n], p['lagtbl_d'][n],
                    p['ktbl_a'][n], p['ktbl_b'][n], p['ktbl_c'][n], p['ktbl_d'][n],
                    p['lagk_lagmax'][n], p['lagk_kmax'][n], p['lagk_qmax'][n],
                    p['lagk_lagmin'][n], p['lagk_kmin'][n], p['lagk_qmin'][n],
                    p['init_co'][n], p['init_if'][n], p['init_of'][n], p['init_stor'][n],
                    self.uptribs[:,n],int(0))

        sim_flow_cfs = np.sum(lagk[0],axis=1)
        
        self.lagk_flow_cfs = pd.Series(sim_flow_cfs, index=self.dates)
        
        return self.lagk_flow_cfs

    def lagk_states_run(self): 
        
        #if n is None:
        #    n=list(range(self.n_uptribs))
        #elif isinstance(n, int):
        #    n=[n]
        #    
        p = self.p['lagk']
                
        states=s.lagk(int(self.dt_hours),int(self.dt_hours),
                    p['lagtbl_a'], p['lagtbl_b'], p['lagtbl_c'], p['lagtbl_d'],
                    p['ktbl_a'], p['ktbl_b'], p['ktbl_c'], p['ktbl_d'],
                    p['lagk_lagmax'], p['lagk_kmax'], p['lagk_qmax'],
                    p['lagk_lagmin'], p['lagk_kmin'], p['lagk_qmin'],
                    p['init_co'], p['init_if'], p['init_of'], p['init_stor'],
                    self.uptribs,int(1))

        state_param=['routed','lag_time','k_inflow','k_storage']
        
        self.lagk_states={}
        for count, param in  enumerate(state_param):
            self.lagk_states[param]=pd.DataFrame(states[count], index=self.dates,columns=self.uptribs_name)
        
        return self.lagk_states


    def sacsnow_run(self,inst=True):

        p = {**self.p['sac'],**(self.p['snow']),**(self.p['uh'])}

        # simulates all zones

        states = s.sacsnow(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'), self.hour.astype('int'),
                        # general pars
                        p['alat'].astype('double'), p['elev'].astype('double'),
                        # sac pars
                        self.sac_pars,
                        # pet and precp adjustments
                        p['peadj'].astype('double'), p['pxadj'].astype('double'),
                        # snow pars
                        self.snow_pars,
                        # initial swe
                        p['init_swe'].astype('double'),
                        # forcings
                        self.forcings.map_fa, self.forcings.ptps_fa, self.forcings.mat_fa,self.forcings.etd,
                        #Pass states option
                        int(0))
        
        tci=states[2]
        
        # channel routing
        self.sacsnow_flow_cfs = self.uh.tci_2_cfs(tci,self.dates,inst=inst)

        #Recalculate FA forcing due to Map and ETD being modified
        self.forcings.fa_ts(self.dt_seconds,self.dates)

        return self.sacsnow_flow_cfs

    def sacsnow_states_run(self,inst=True):

        p = {**self.p['sac'],**(self.p['snow']),**(self.p['uh'])}

        # simulates all zones
        states = s.sacsnow(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'), self.hour.astype('int'),
                        # general pars
                        p['alat'].astype('double'), p['elev'].astype('double'),
                        # sac pars
                        self.sac_pars,
                        # pet and precp adjustments
                        p['peadj'].astype('double'), p['pxadj'].astype('double'),
                        # snow pars
                        self.snow_pars,
                        # initial swe
                        p['init_swe'].astype('double'),
                        # forcings
                        self.forcings.map_fa, self.forcings.ptps_fa, self.forcings.mat_fa,self.forcings.etd,
                        #Pass states option
                        int(1))

        state_param=['map_pxadj','etd_adj','tci','aet',
                        'uztwc','uzfwc','lztwc','lzfsc','lzfpc','adimc',
                        'roimp', 'sdro', 'ssur', 'sif', 'bfs', 'bfp',
                        'swe','aesc','neghs','liqw','raim','psfall','prain']
        self.sacsnow_states={}
        for count, param in  enumerate(state_param):
            self.sacsnow_states[param]=pd.DataFrame(states[count], index=self.dates,columns=self.zones)

        #Calculate streamflow for each zone
        sf_df=pd.DataFrame()
        for count, zone in enumerate(self.zones):
            tci_zone=self.sacsnow_states['tci'][zone].astype('double').to_numpy()
            tci_zone=np.expand_dims(tci_zone,axis=1)
            tci_zone=np.asfortranarray(tci_zone)
            sf_zones=self.uh.tci_2_cfs(tci_zone,self.dates,count,inst=inst).rename(zone)
            sf_df=pd.concat([sf_df,sf_zones],axis=1,ignore_index=True)
        sf_df.index=self.dates
        sf_df.columns=self.zones
        
        self.sacsnow_states['sf']=sf_df
        
        #Recalculate FA forcing due to Map and ETD being modified
        self.forcings.fa_ts(self.dt_seconds,self.dates)
        
        return self.sacsnow_states
    
    def consuse_run(self):

        p = self.p['consuse']
        cms_2_cfs=35.3147
        
        #Get natural flow
        #Create blank simulation series
        qnat=pd.Series(0,index=self.dates)
        
        #If there are sac/snow zone, calculate runoff
        if self.n_zones > 0:
            qnat = qnat+self.sacsnow_run(inst=True)
        
        #If there are upstream reaches to route, add them to the total flow
        if self.n_uptribs > 0:
            qnat = self.lagk_run() + qnat

        #Chanloss Adjustment
        qnat=self.chanloss(qnat)

        #Convert to daily using the weighting scheme that CHPS uses of utilizing 5 points (edges assigned .5)
        qnat_daily=(qnat.rolling(5,center=True).sum()+qnat.rolling(3,center=True).sum())/8
        qnat_daily=qnat_daily.loc[qnat_daily.index.hour==12]
        qnat_daily=qnat_daily.resample('1D').sum()
        
        #Get PET
        pet=pd.DataFrame(self.forcings.pet,columns=self.zones,index=self.dates)
        
        #Create a blank state dataframe
        state_param=['QADJ','QDIV','QRF_in','QRF_out','QOL','QCD','CE','RFSTOR']
        self.consuse_states={}
        for count, param in  enumerate(state_param):
            self.consuse_states[param]=pd.DataFrame()
        
        #Run consuse for each zone individualys
        for n, cu_name in zip(range(self.n_consuse), self.consuse_name):
            
            #Get PET from equivalent SAC zone.  
            #NOTE:  To match CHPS results have to be shifted back 1 hr so 00:00 timestep
            #       is included in previous day
            pet_daily=pet[cu_name].shift(periods=-1, freq='H').resample('1D').sum()

            consuse_ts_input=pd.concat([pet_daily,qnat_daily],axis=1)
            consuse_ts_input.columns=['pet','qnat']
            consuse_ts_input=consuse_ts_input[~consuse_ts_input.isna().any(axis=1)]\
            
            peadj=self.pars.loc[(self.pars.name=='peadj')&(self.pars.zone==cu_name),'value'].squeeze()
            
            dates_input=consuse_ts_input.index
            
            consuse_ts_input=consuse_ts_input.astype('double').to_numpy()
            consuse_ts_input=np.asfortranarray(consuse_ts_input)
            
            #pet_iput=consuse_ts_input.pet.astype('double').to_numpy()
            #pet_input=np.asfortranarray(pet_input)
            
            #qnat_iput=consuse_ts_input.qnat.astype('double').to_numpy()
            #qnat_input=np.asfortranarray(qnat_input)
            
            states=s.consuse(dates_input.year.astype('int'), dates_input.month.astype('int'), dates_input.day.astype('int'),
                         p['area_km2'][n].astype('double'),p['irr_eff'][n].astype('double'),np.double(p['min_flow_cmsd'][n]*cms_2_cfs),
                         p['rf_accum_rate'][n].astype('double'),p['rf_decay_rate'][n].astype('double'),
                         self.peadj_cu[:,n],peadj,
                         consuse_ts_input[:,0],consuse_ts_input[:,1])
            
            #Concat state value for CU zone to dictionary. IF QADJ 
            for count, param in  enumerate(state_param):
                if param=='QADJ':
                    self.consuse_states[param]=pd.DataFrame(states[count], index=dates_input,columns=[param])
                else:
                    self.consuse_states[param]=pd.concat([self.consuse_states[param],
                            pd.DataFrame(states[count], index=dates_input,columns=[cu_name])],axis=1)
            #Update the qnat to reflect the adjusted flow (needed for basins w/multiple CU zones)
            qnat_daily=self.consuse_states['QADJ']
        
        return self.consuse_states

    def chanloss(self,sim_sf):
    
        #Check if there is a chanloss module
        if self.n_chanloss==0:
            sim_sf_adj=sim_sf
        else:
            p = self.p['chanloss']
            
            periods=np.array([p['cl_period_start'],p['cl_period_end']],dtype='int')
            sim_sf_adj=s.chanloss(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'),
                        p['cl_factor'],periods,p['cl_type'].astype('int'),
                        sim_sf.astype('double').to_numpy())
            sim_sf_adj=pd.Series(sim_sf_adj, index=self.dates)
        return sim_sf_adj

    def run_all(self,inst=True):
        
        #Create blank simulation series
        self.sim=pd.Series(0,index=self.dates)
        
        #If there are sac/snow zone, calculate runoff
        if self.n_zones > 0:
            self.sim = self.sim+self.sacsnow_run(inst=True)
        
        #If there are upstream reaches to route, add them to the total flow
        if self.n_uptribs > 0:
            self.sim = self.lagk_run() + self.sim

        #Chanloss Adjustment
        self.sim=self.chanloss(self.sim)

        #If there area CONSUSE areas, adjust the flow
        if self.n_consuse > 0:
            self.consuse_run()
            qnat_cu_adj=self.consuse_states['QDIV'].sum(axis=1)-self.consuse_states['QRF_out'].sum(axis=1)
            #Shift forward a day so that the correct adjustement is applied  to the correct day
            qnat_cu_adj.index=qnat_cu_adj.index+pd.Timedelta(1, unit='D')
            #Backfill to fill all values after 00:00 and forward fill to correct missing values at end of timeseries
            qnat_cu_adj=qnat_cu_adj.reindex(self.sim.index).backfill().ffill()
            q_adj=self.sim - qnat_cu_adj
            self.sim = self.sim - qnat_cu_adj
            #Replace any negative values with zero
            self.sim[self.sim < 0]=0

        #return instantaneous or period avg depending on chosen option
        if not inst:
            current_sim=self.sim.to_numpy().flatten()
            next_sim = pd.DataFrame(self.sim).shift(-1).to_numpy().flatten()
            sim_flow_pavg_cfs = (current_sim + next_sim) / 2
            self.sim = pd.Series(sim_flow_pavg_cfs, index=self.dates)

        return self.sim
        
class Forcings:

    def __init__(self,
                 forcings: list,
                 pars: pd.DataFrame):
        
        #Filter parmater table to only include those relevant to  FA 
        self.pars=pars.loc[(pars.type=='fa')|(pars.type=='fa_limit')|
                        (pars.name=='zone_area')|(pars.name=='alat')]
        
        #Get number of zones and names
        zones = self.pars.loc[(self.pars['zone'].str.contains('-'))|(self.pars['zone'].str.contains('_'))].zone.unique()
        self.zones=np.sort(zones)
        n_zones = len(zones)
        sim_length = forcings[0].shape[0]

        # Timestep in different units
        dt_seconds = int((forcings[0].index[1] - forcings[0].index[0]).total_seconds())

        # Extract vectors as numpy arrays for speed
        dates = forcings[0].index

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
                self.peadj_m[i, j] = pars[(pars['name'] == 'peadj_' + f'{m:02}') & (pars['zone'] == z) &
                        (pars['type']== 'sac')]['value'].astype('double').to_numpy()[0]

        self.map_fa_limits=np.asfortranarray(self.map_fa_limits)
        self.mat_fa_limits=np.asfortranarray(self.mat_fa_limits)
        self.ptps_fa_limits=np.asfortranarray(self.ptps_fa_limits)
        self.pet_fa_limits=np.asfortranarray(self.pet_fa_limits)
        self.peadj_m=np.asfortranarray(self.peadj_m)

        #Make dummy climo input
        self.climo=np.full((12, 4), -9999)
        self.climo=np.asfortranarray(self.climo)

        # Create forcing arrays
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
        

    def fa_ts(self,dt_sec,dates):

        dt=int(dt_sec)
        year = dates.year.astype('int')
        month = dates.month.astype('int')
        day = dates.day.astype('int')
        hour = dates.hour.astype('int')
        
        alat=self.pars.loc[self.pars.name.str.contains('alat')].sort_values(by='zone').value.to_numpy().astype('double')
        area=self.pars.loc[self.pars.name.str.contains('zone_area')].sort_values(by='zone').value.to_numpy().astype('double')
        
        fa=s.fa_ts(dt,year,month,day,hour,
                    alat,area,
                    self.peadj_m,
                    self.map_fa_pars,self.mat_fa_pars,self.pet_fa_pars,self.ptps_fa_pars,
                    self.map_fa_limits,self.mat_fa_limits,self.pet_fa_limits,self.ptps_fa_limits,
                    self.climo,
                    self.map,self.ptps,self.mat)

        self.map_fa,self.mat_fa,self.ptps_fa,self.pet,self.etd = fa[5:]
        
        #Create a dictionary of dataframes for output purposes
        fa_param=['map_fa','mat_fa','ptps_fa','pet_fa','etd_fa']
        fa_dict={}
        for count, param in  enumerate(fa_param):
            fa_dict[param]=pd.DataFrame(fa[count+5], index=dates,columns=self.zones)
        return fa_dict
    
    def fa_fac(self,dt_sec,dates):

        dt=int(dt_sec)
        year = dates.year.astype('int')
        month = dates.month.astype('int')
        day = dates.day.astype('int')
        hour = dates.hour.astype('int')
        
        alat=self.pars.loc[self.pars.name.str.contains('alat')].sort_values(by='zone').value.to_numpy().astype('double')
        area=self.pars.loc[self.pars.name.str.contains('zone_area')].sort_values(by='zone').value.to_numpy().astype('double')
        
        fac=s.fa_ts(dt,year,month,day,hour,
                    alat,area,
                    self.peadj_m,
                    self.map_fa_pars,self.mat_fa_pars,self.pet_fa_pars,self.ptps_fa_pars,
                    self.map_fa_limits,self.mat_fa_limits,self.pet_fa_limits,self.ptps_fa_limits,
                    self.climo,
                    self.map,self.ptps,self.mat)
        fac=fac[1:5]
        
        #Create a dataframe for output return, ignore first output aray as it is climo not a fac adjustment
        fac_parameters=['map_fac','mat_fac','pet_fac','ptps_fac']
        fac_df=pd.DataFrame(columns=fac_parameters,index=range(1,13))
        for i, col in enumerate(fac_parameters,0):
            fac_df[col]=fac[i]
        
        return fac_df
        

class UH:

    def __init__(self,
                 pars: pd.DataFrame,
                 dt_hrs: float):
                 


        #Filter paramter table to only inclue UH type
        self.pars=pars.loc[(pars.type=='uh')]

        #Get number of zones and names
        zones = self.pars.zone.unique()
        self.zones=np.sort(zones)
        self.n_zones = len(zones)

        #Get model timestep in hours
        self.dt_hours=dt_hrs

        #Get parameter values
        self.p = {}
        for par in self.pars.name.unique():
            self.p[par] = self.pars.loc[self.pars['name'] == par].sort_values(by='zone')['value'].to_numpy()
        
        #Check if scale parameter is provide, if not calculate
        if 'unit_scale' not in self.p.keys():
            #populate the p instance
            self.p['unit_scale']=np.zeros(self.n_zones)
            for i in range(self.n_zones):
                
                shape=self.p['unit_shape'][i]
                
                toc_gis=self.p['unit_toc'][i]
                toc_adj=self.p['unit_toc_adj'][i]
                toc=toc_gis*toc_adj
                
                scale=s.uh2p_get_scale_root(float(shape),float(toc),float(1))
                
                self.p['unit_scale'][i]=scale
            #populate the pars instance
            scale_df=self.pars.loc[self.pars.name=='unit_shape'].sort_values('zone').copy()
            scale_df['value']=self.p['unit_scale']
            scale_df.replace('shape','scale',regex=True,inplace=True)
            self.pars=pd.concat([self.pars,scale_df],axis=0)
        
        #Calculate the UH for each zone
        uh_df=pd.DataFrame()
        for i in range(self.n_zones):
            shape=self.p['unit_shape'][i]
            scale=self.p['unit_scale'][i]
            area=self.p['zone_area'][i]
            #volume calcuation zone_area_km2 to mi2 (0.386102) to ft2 (0.386102*5280)
            #multiplied by 1 inch (in ft) for a volume of ft3
            total_uh_vol=area*0.386102*5280**2*1/12

            #dimensionless uh
            uh_dl=s.uh2p_call(float(shape),float(scale),float(self.dt_hours),int(1000))
            first0 = next(x for x, val in enumerate(uh_dl) if val == 0)
            uh_dl=uh_dl[:first0]
            
            #Distribute volume
            uh_vol= [ordinate * total_uh_vol for ordinate in uh_dl]
            #Divide by model timestep in seconds
            uh= [ordinate / (self.dt_hours*60**2) for ordinate in uh_vol]
            #Assign to UH dataframe
            uh_df=pd.concat([uh_df,pd.Series(uh,name=self.zones[i])],axis=1)
        #modify and label index
        uh_df.index=uh_df.index.to_series().multiply(self.dt_hours)
        uh_df.index.rename('hours',inplace=True)
        
        #create unit_hydrograph instance
        self.unit_hydrograph=uh_df
        
        
    def tci_2_cfs(self,tci,dates,n=None,inst=True):
        
        if n is None:
            n=list(range(self.n_zones))
        elif isinstance(n, int):
            n=[n]
        
        m_uh = 1000  # max UH length
        n_uh = len(tci) + m_uh
        sim_flow_inst_cfs = np.zeros(len(tci))
        
        for y, z in zip(range(len(tci[0])),n):
            
            shape=self.p['unit_shape'][z]
            scale=self.p['unit_scale'][z]
            
            flow_routed = s.duamel(tci[:, y], float(shape), float(scale),
                                   float(self.dt_hours/24), int(n_uh), int(m_uh), int(1), int(0))

            # flow_routed units:  mm, zone_area units:  km2,  1000 is a combined conversion of km2->m2 and mm->m
            # flow routed is depth of runoff over a basin for a time step. that with area converts it to a volume
            # and dt.second (timestep in sec) is used to complete conversion of runoff to flow
            # instantaneous routed flow weighted by zone area
            sim_flow_inst_cfs = sim_flow_inst_cfs + flow_routed[0:len(tci)] * 1000 * 3.28084 ** 3 / \
                                (self.dt_hours*60**2) * self.p['zone_area'][z]
        
        #return instantaneous or period avg depending on chosen option
        if inst:
            sim_flow_cfs = pd.Series(sim_flow_inst_cfs, index=dates)
        else:
            next_sim = pd.DataFrame(sim_flow_inst_cfs).shift(-1).to_numpy().flatten()
            sim_flow_pavg_cfs = (sim_flow_inst_cfs + next_sim) / 2
            sim_flow_cfs = pd.Series(sim_flow_pavg_cfs, index=dates)
        
        #UH output needs to be shifted forward 6 hrs because of how forcings are treated in AutoCalb
        #Repeat the first flow data point and append to the beginning of the ts, so there is no loss in a timestep
        sim_flow_cfs.loc[sim_flow_cfs.index[0] - pd.offsets.Hour(int(self.dt_hours))]=sim_flow_cfs[0]
        sim_flow_cfs.sort_index(inplace=True)
        #Shift the data forward 6hrs
        sim_flow_cfs.index=sim_flow_cfs.index+pd.Timedelta(self.dt_hours, unit='H')
        #Cutting off the last data point to keep the same length as prior
        sim_flow_cfs=sim_flow_cfs[:-1]
        
        return sim_flow_cfs