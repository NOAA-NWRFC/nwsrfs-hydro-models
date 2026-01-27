import os, sys
from typing import NewType
import pandas as pd
import numpy as np
from .wrapper import nwsrfs_src as s
from . import nwsrfs
from . import utils
#import pdb; pdb.set_trace()

class nwsrfs_prep:

    '''
    This function reads in a files created by the NWRFC autocalibration process to DataFrames stored as attribues.  

    Files must must follow file conventions of the NOAA-NWRFC/nwsrfs-hydro-autocalibration repository optimization tools. Forcing csv files
    must be in the directory (forcing_por_*), even if it a routing only reach (i.e. no local inflow modeled by SAC-SMA, Snow17, UH).

    If no run_dir is provided, the first "results_*" directory found within the autocalb_dir path will be used.

    Attributes:

    autocalb_dir (str): Path to a NWRFC autocalibration directory
    run_dir (str | None): Name of optimization run subdirectory within the autocal_dir.  postprocess.R needs to already been ran for subdirectory.  
        If ''None'' provided, defaults to using first "results_*" directory found within the autocalb_dir 
    '''

    def __init__(self,
                autocalb_dir: str, 
                run_dir: str | None = None):

            #Check if directory exist and assign
            if run_dir is not None:
                check_path = os.path.join(autocalb_dir,run_dir)
            else:
                check_path = autocalb_dir
            self.autocalb_dir = autocalb_dir
            self._autocalb_dir_basename = os.path.basename(self.autocalb_dir)

            if not os.path.isdir(check_path):
                msg = f'{check_path} is not a directory.'
                raise ValueError(msg)

            #Search autocalb directory for csv files.
            self.autocalb_files = self._create_dir_df(self.autocalb_dir)

            #Assign run_dir if not specified
            if run_dir is not None:
                self.run_dir = run_dir
            else:
                results_dir_query = self.autocalb_files.loc[self.autocalb_files.folder.str.startswith('results'),'folder'].sort_values().unique()

                if len(results_dir_query) == 0:
                    raise ValueError(f"{autocalb_dir} contains no results_* folder.")

                self.run_dir = results_dir_query[0]
                msg = f'Defaulting to using the following results directory: {self.run_dir}'
                print(msg)

            #Filter out files that are not in either autocalb_dir or the run_dir
            self.autocalb_files = self.autocalb_files.loc[self.autocalb_files.folder.isin([self._autocalb_dir_basename,self.run_dir])]

            #Validate autocalb_dir and run_dir contents
            self.validate_contents()

            #Read in files
            self.load_files()

            #Catalog nwsrfs model being utilized
            self.interrogate_pars()

            #Extract time vectors
            self.dates = self.forcings_raw['map'].index.strftime('%Y-%m-%d %H:00').to_numpy()
            self.year = self.forcings_raw['map'].index.year.to_numpy()
            self.month = self.forcings_raw['map'].index.month.to_numpy()
            self.day = self.forcings_raw ['map'].index.day.to_numpy()
            self.hour = self.forcings_raw['map'].index.hour.to_numpy()

            #Model timestep in hours
            self.dt_hours = int(utils._define_timestep_sec(self.year, self.month, self.day, self.hour)/3600)

            # If a routing only reach only set forcings to None, otherwise assign zones to each DataFrame's column names
            if not self.sacsnow_logic:
                self.forcings_raw = None
            else:
                self.forcings_raw = {key: df.set_axis(self.zone_names, axis=1) for key, df in self.forcings_raw.items()}

            #If a routing component, assign upflow names to columns
            if self.upflow_logic:
                self.upflow = self.upflow.set_axis(self.upflow_names, axis=1)


    def validate_contents(self):

        '''
        Validates that necessary files and within ``autocalb_dir``, and checks which auxilary file are present
        '''

        #Check for optimial parameter file
        self.pars_path = os.path.join(self.autocalb_dir,self.run_dir,'pars_optimal.csv')
        if not os.path.isfile(self.pars_path):
            msg = f'{self.pars_path} is missing'
            raise ValueError(msg)

        #Check for flow_daily files
        self._daily_flow_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('flow_daily')]
        if len(self._daily_flow_df) == 0:
            msg = 'Daily flow csv file is missing.  File must start with flow_daily_*.'
            raise ValueError(msg)
        elif len(self._daily_flow_df) > 1:
            msg = 'Daily flow csv file is ambiguous. Only one csv file must start with flow_daily_*.'
            raise ValueError(msg)
        self.daily_flow_path = os.path.join(self._daily_flow_df.path.squeeze(),self._daily_flow_df.file_name.squeeze())

        #Check for forcing files
        self.forcings_por_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('forcing_por')].copy()
        self.forcings_por_df.sort_values(by='file_name',inplace=True)
        if len(self.forcings_por_df) == 0:
            msg = 'POR forcing csv files are missing. Files must start with forcing_por_*.'
            raise ValueError(msg)

        #Check for instantaneous flow file 
        self._inst_flow_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('flow_instantaneous')]
        if len(self._inst_flow_df) == 0:
            self.inst_flow_logic = False
            self.inst_flow_path = None
        if len(self._inst_flow_df) == 1:
            self.inst_flow_logic = True
            self.inst_flow_path = os.path.join(self._inst_flow_df.path.squeeze(),self._inst_flow_df.file_name.squeeze())
        else:
            msg = 'Instantaneous flow csv file is ambiguous. Only one csv file must start with flow_instantaneous_*.'
            raise ValueError(msg)

        #Check for upflow flow files
        self.upflow_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('upflow')].copy()
        self.upflow_df.sort_values(by='file_name',inplace=True)
        if len(self.upflow_df) == 0:
            self.upflow_logic = False
        else:
            self.upflow_logic = True
     
    def load_files(self):
       
        drop_cols = ['year','month','day','hour']

        #Read pars file
        self.pars=pd.read_csv(self.pars_path)
        self.pars = self.pars.sort_values(['name', 'zone'])

        #Make par file edits for CHANLOSS
        self._cl_parfile_edits()

        #Read daily flow file
        self.daily_flow = pd.read_csv(self.daily_flow_path)
        self.daily_flow.index = pd.to_datetime(self.daily_flow[['year', 'month', 'day']])
        #Added errors='ignore' to ignore missing hour column
        self.daily_flow.drop(drop_cols,axis=1,inplace=True,errors='ignore')
        self.daily_flow =self.daily_flow.resample('6h').ffill()

        #Read forcing file data as DataFrame and reconstruct as a dictionary with each forcing as a key
        forcings_import = self._read_tsfiles(self.forcings_por_df, drop_cols)
        self.forcings_raw = {'map': pd.DataFrame(index=forcings_import[0].index),
                             'mat': pd.DataFrame(index=forcings_import[0].index),
                             'ptps': pd.DataFrame(index=forcings_import[0].index)}
        for i in range(len(forcings_import)):
                self.forcings_raw['map'] = pd.concat([self.forcings_raw['map'],forcings_import[i].loc[:,'map_mm'].rename(f'zone_{str(i)}')],axis=1)
                self.forcings_raw['mat'] = pd.concat([self.forcings_raw['mat'],forcings_import[i].loc[:,'mat_degc'].rename(f'zone_{str(i)}')],axis=1)
                self.forcings_raw['ptps'] = pd.concat([self.forcings_raw['ptps'],forcings_import[i].loc[:,'ptps'].rename(f'zone_{str(i)}')],axis=1)


        #If exists, read in instantaneous flow file
        if self.inst_flow_logic:
            self.inst_flow = pd.read_csv(self.inst_flow_path)
            self.inst_flow.index = pd.to_datetime(self.inst_flow[['year', 'month', 'day', 'hour']])
            self.inst_flow.drop(drop_cols,axis=1,inplace=True)
        else:
            self.inst_flow = None

        #If exists, read in upflow flow files and reconstruct as a single DataFrame
        if self.upflow_logic:
            upflow_import = self._read_tsfiles(self.upflow_df, drop_cols)
            self.upflow = pd.DataFrame(index=upflow_import[0].index)
            for i in range(len(upflow_import)):
                self.upflow  = pd.concat([self.upflow,upflow_import[i].flow_cfs.rename(f'upflow_{i}')],axis=1)
        else:
            self.upflow = None

    def interrogate_pars(self):

        '''
        Using the parfile, gathers number of zones, zone names, number of upstream points, upstream point names, if CONSUSE is used, and/or if CHANLOSS is used
        '''

        #Get zone names and number , if present
        self.zone_names = tuple(self.pars.loc[(self.pars['zone'].str.contains('-'))|(self.pars['zone'].str.contains('_'))].zone.unique())
        self.n_zones = len(self.zone_names)
        if self.n_zones > 0:
            self.sacsnow_logic = True
        else:
            self.sacsnow_logic = False
        
        #Catalog routing, if present
        if self.upflow_logic:
           self.upflow_names = tuple(self.pars.loc[self.pars.type=='lagk'].zone.unique())
           self.n_upflow = len(self.upflow_names)
        else:
           self.upflow_names = None
           self.n_upflow = 0

        #Catalog CONSUSE, if present
        if self.pars.zone.str.contains('-CU').any():
            self.consuse_logic = True
            self.consuse_name = tuple(self.pars.loc[self.pars.type=='consuse'].zone.unique())
            self.n_consuse = len(consuse_name)
        else:
            self.consuse_logic = False
            self.consuse_name = None
            self.n_consuse=0

        #Catalog CHANLOSS info if present
        if self.pars.zone.str.contains('_CL').any():
            self.chanloss_logic = True
        else:
            self.chanloss_logic = False

    def _cl_parfile_edits(self):
        '''
        Makes changes to ``pars`` DataFrame in regards to CHANLOSS parameter row's 
        naming convention and parameters provided
        '''
        #Rename Zones for each CL Module.  But do not rename cl_type, n_clmods,min_q
        cl_exclude_logic=(self.pars.type=='chanloss')&(~self.pars.name.isin(['cl_type','n_clmods','cl_min_q']))
        self.pars.loc[cl_exclude_logic,['zone']]=self.pars.loc[cl_exclude_logic].zone + \
                                '_CL'+ self.pars.loc[cl_exclude_logic].name.str.split('_').str[-1]   
        #n_clmods pars row now unnecessary
        self.pars.drop(self.pars.loc[self.pars.name=='n_clmods'].index,inplace=True)
        #Remove the CL module name from the name columns, except for cl_min_q
        cl_remove_logic = (self.pars.type=='chanloss')&(self.pars.name!='cl_min_q')
        self.pars.loc[cl_remove_logic ,['name']]= \
                                self.pars.loc[cl_remove_logic].name.str.split('_').str[:-1].str.join('_')
        #Correct the cl_type name
        self.pars.loc[self.pars.p_name.str.contains('cl_type'),['name']]='cl_type'

    @staticmethod
    def _create_dir_df(path:str):
        '''
        Makes changes to ``pars`` DataFrame in regards to CHANLOSS parameter row's 
        naming convention and parameters provided

        Args:
            path (str):  Path to directory with a basin nwrfc autocalibration run(s).
        '''
        autocalb_files=pd.DataFrame(columns=['path','file_name']) 
        n=1
        for root, dirs, files in os.walk(path):
            for file in files:
                if '.csv' in file:
                    autocalb_files=pd.concat([autocalb_files,
                                              pd.DataFrame({'path':root,
                                                           'file_name':file},index=[n])],axis=0)
                    n=n+1

        autocalb_files['folder']=autocalb_files.path.apply(os.path.basename)

        return autocalb_files

    @staticmethod
    def _read_tsfiles(path_ts:str, drop_cols: list[str]):
        '''
        Reads in csv timeseries files produced via the nwrfc autocalibration run(s) as a
        pandas DataFrame.

        Args:
            path_ts (str):  Path to csv timeseries file.  File is expected to have ['year', 'month', 'day', 'hour'] columns.
            drop_cols (list[str]):  List of string with columns to drop from dataframe

        '''
        list_df = []
        for index, row in path_ts.iterrows():
            ts_df = pd.read_csv(os.path.join(row.path,row.file_name))
            ts_df.index=pd.to_datetime(ts_df[['year', 'month', 'day', 'hour']])
            ts_df.drop(drop_cols,axis=1,inplace=True)
            list_df.append(ts_df)
        return list_df


class nwsrfs_run(nwsrfs_prep,nwsrfs.fa,nwsrfs.sacsnow,nwsrfs.gamma_uh):
    '''

    ###what does this class do?!?#####

    If no run_dir is provided, the first "results_*" directory found within the autocalb_dir path will be used.

    Attributes:

    autocalb_dir (str): Path to a NWRFC autocalibration directory
    run_dir (str | None): Name of optimization run subdirectory within the autocal_dir.  postprocess.R needs to already been ran for subdirectory.  
        If ''None'' provided, defaults to using first "results_*" directory found within the autocalb_dir
    forcing_adj (bool | list[str]):  If ``True`` monthly climatological forcing adjustments will be applied to all forcings.  Alternatively, a list with
        with specific forcing to apply climatological forcing adjustments can be supplied: 'map','mat', 'ptps','pet' 
    '''

    def __init__(self,
                autocalb_dir: str, 
                run_dir: str | None = None,

                forcing_adj: bool | list[str] = True):

        #Initiate nwsrfs_prep    stop

        nwsrfs_prep.__init__(self, autocalb_dir, run_dir)

        #Validate forcing_adj argument
        self._interrogate_fa_arg(forcing_adj)

        #If there is a sac-snow model, perform forcing adjustments, SAC-SMA, SNOW17, UH Gamma
        if not self.sacsnow_logic:
            self.fa_factors = None            
            self.forcings_climo = None
            self.return_sf = None
        else:
            self.fa_run()
            self.sacsnow_uh_run()

        ############################################


        #     #Calculate the UH using a dedicated model class
        #     self.uh=UH(self.pars,self.dt_hours)
        # else:
        #     self.forcings = np.nan
        #     self.uh = np.nan

        # #format peadj for consuse calculation 
        # self.peadj_cu = np.full([12, n_consuse], np.nan)
        # for i in range(12):
        #     m = i + 1
        #     for j, z in zip(range(n_consuse), consuse_name):
        #         self.peadj_cu[i, j] = pars[(pars['name'] == 'peadj_cu_' + f'{m:02}') & (pars['zone'] == z) &
        #                 (pars['type']== 'consuse')]['value'].astype('double').to_numpy()[0]
        # self.peadj_cu=np.asfortranarray(self.peadj_cu)
        


    def _interrogate_fa_arg(self,forcing_adj):
        '''
        Add approprate attributes regarding climatological forcing adjustments as specificed by the forcing_adj argument and if 
        SAC-SMA, Snow17, UH are being utilized.

        Args:
            forcing_adj (bool | list[str]):  If ``True`` monthly climatological forcing adjustments will be applied to all forcings.  Alternatively, a list with
                with specific forcing to apply climatological forcing adjustments can be supplied: 'map','mat', 'ptps','pet' 
        '''

        #Validate forcing_adj argument
        if not self.sacsnow_logic:
                self.forcing_adj_logic = None
                self.forcing_adj_types = ()
        elif isinstance(forcing_adj,bool):
            if forcing_adj:
                self.forcing_adj_logic = True
                self.forcing_adj_types = ('map','mat', 'ptps','pet')
            else:
                self.forcing_adj_logic = None
                self.forcing_adj_types = None
        else:
            self.forcing_adj_logic = True
            #Just in case, set string entries to lower case and remove duplicates entries
            forcing_adj = [s.lower() for s in forcing_adj]
            forcing_adj = list(set(forcing_adj))

            #Check if forcing_adj string inputs match xpected forcing types
            forcing_types = {'map','mat','ptps','pet'}
            validate_types = set(forcing_adj).issubset(forcing_types)
            #validate_types is true than assign forcing_adj_types else return error
            if validate_types:
                self.forcing_adj_types = tuple(forcing_types.intersection(forcing_adj))
            else:
                msg = f'One or more string forcing_adj argument string not understood, expecting: {", ".join(forcing_types)}'
                raise ValueError(msg)   

    def fa_run(self,         
            validate:bool = True):
        '''
        Apply monthly climatological forcing adjustments to forcing specified by the ``forcing_adj_types`` attribute
        
        Args:
            validate (bool): Validate that map, mat, ptps, and pet inputs are correct format/type. Default: True
        '''

        #If forcing_adj_types is None or then set all fa_pars values to scale=1, p_redist=0, std=10, shift=0
        nofa_pars = np.array([1,0,10,0])
        #fa adjustment parameters:  scale, p_redist, std, shift 
        df_2_dict = dict(zip(self.pars.loc[self.pars.type == 'fa'].name,self.pars.loc[self.pars.type == 'fa'].value))
        fa_pars = {}
        for f in ['map','mat','ptps','pet']:
            #Turn off fa adjustments for all forcings not within forcing_adj_types attribute 
            if f not in self.forcing_adj_types:
                fa_pars[f] = nofa_pars
            #Otherwise use forcing adjustment parameters provided in pars
            else:
                fa_pars[f] = np.array([df_2_dict[f'{f}_scale'], df_2_dict[f'{f}_p_redist'],
                                          df_2_dict[f'{f}_std'], df_2_dict[f'{f}_shift']])
            
        # monthly forcing adjustments
        fa_limits = {key: np.full([12, 2], np.nan)  for key in ['map','mat','ptps','pet']}
        peadj_m = np.full([12, self.n_zones], np.nan)
        peadj = {key:None for key in ['map','mat','ptps']}
        
        for i in range(12):
            m = i + 1
            for f in ['map','mat','ptps','pet']:
                fa_limits[f][i, 0] = self.pars[(self.pars.name == f'{f}_lower_{m:02}')&(self.pars.zone==self.zone_names[0])].value.item()
                fa_limits[f][i, 1] = self.pars[(self.pars.name == f'{f}_upper_{m:02}')&(self.pars.zone==self.zone_names[0])].value.item()
            for j, z in zip(range(self.n_zones), self.zone_names):
                #import pdb; pdb.set_trace()
                peadj_m[i, j] = self.pars.loc[(self.pars.name == f'peadj_{m:02}') & (self.pars.zone == z) &
                        (self.pars.type == 'sac')].value.item()
        peadj['pet'] = peadj_m

        #Make dummy climo input
        climo = None

        # Create forcing arrays
        forcings_raw = {}
        for f in ['map','mat','ptps']:
            forcings_raw[f] = self.forcings_raw[f].to_numpy()
        forcings_raw['pet'] = None

        #Get alat and area parameters
        alat=self.pars.loc[self.pars.name.str.contains('alat')].value.to_numpy()
        area=self.pars.loc[self.pars.name.str.contains('zone_area')].value.to_numpy()

        #Create a dictionary for each forcings dataclass
        fa_dc = {}
        for f in ['map','mat','ptps','pet']:
            fa_dc[f] = nwsrfs.fa_pars(year = self.year,month =self.month,day = self.day,hour = self.hour,
                            pars = fa_pars[f],
                            area = area,
                            alat = alat,
                            limits = fa_limits[f],
                            forcings = forcings_raw[f],
                            peadj_m = peadj[f],
                            climo = climo)

        #Initiate fa wrapper class
        nwsrfs.fa.__init__(self,
            map_dataclass = fa_dc['map'],
            mat_dataclass = fa_dc['mat'],
            ptps_dataclass = fa_dc['ptps'],
            pet_dataclass = fa_dc['pet'],
            validate = validate)

    @property
    def forcings(self) -> dict[str, pd.DataFrame]:
        '''
        Returns a dictionary of adjusted forcings as DataFrames. 

        The dictionary keys are:
        * **map**: Precipitation (units: mm) 
        * **mat**: Air temperature (units: degc)
        * **ptps**: Fraction of precipitation as snow (units: fraction 0-1)
        * **etd**: Evaporation demand (units: mm)
        '''
        
        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.sacsnow_logic:
            return None

        #Get forcings from fa class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.fa.forcings.fget(self)

        #Remove "_fa" from dictionary keys, then remove key/value pair 'pet'
        fixed_output = {key.split('_')[0]:value for key, value in raw_output.items()}
        fixed_output.pop('pet')
        
        return fixed_output


    def sacsnow_uh_run(self, validate:bool=True):
        '''
        Run SAC-SMA, SNOW17, and gamma UH with the parameters specified in ``pars``.

        Args:
            validate (bool): Validate that SAC-SMA, Snow17, and gamma UH inputs are correct format/type. Default: True
        '''

        #Get a nested dictionary for SAC-SMA, Snow17, and gamma UH with parameter values
        pars_dict = {}
        for par_type in ['sac','snow','uh']:
            pars_dict[par_type]= {}
            for par in self.pars.loc[self.pars.type==par_type].name.unique():
                pars_dict[par_type][par] = self.pars.loc[(self.pars.type==par_type)&
                    (self.pars['name'] == par)].sort_values(by='zone')['value'].to_numpy()

        #Apply toc adjutment to toc parameter
        toc = pars_dict['uh']['unit_toc'] * pars_dict['uh']['unit_toc_adj'] 

        #Initate gammauh_pars data class
        gammauh_dc = nwsrfs.gammauh_pars(dt_hours = self.dt_hours, area = pars_dict['uh']['zone_area'], 
                        shape = pars_dict['uh']['unit_shape'], toc = toc)

        #Initiate gamma_uh wrapper class
        nwsrfs.gamma_uh.__init__(self,
            pars_dataclass = gammauh_dc,
            validate = validate)        

        #Format required dataclass inputs sac_pars and snow_pars
        sac_pars = np.concatenate([[pars_dict['sac']['uztwm']], [pars_dict['sac']['uzfwm']], [pars_dict['sac']['lztwm']],
                                    [pars_dict['sac']['lzfpm']],[pars_dict['sac']['lzfsm']], [pars_dict['sac']['adimp']],
                                    [pars_dict['sac']['uzk']],[pars_dict['sac']['lzpk']], [pars_dict['sac']['lzsk']],
                                    [pars_dict['sac']['zperc']],[pars_dict['sac']['rexp']], [pars_dict['sac']['pctim']],
                                    [pars_dict['sac']['pfree']],[pars_dict['sac']['riva']], [pars_dict['sac']['side']],
                                    [pars_dict['sac']['rserv']],[pars_dict['sac']['efc']]
                                    ],axis=0)
        
        snow_pars = np.concatenate([[pars_dict['snow']['scf']], [pars_dict['snow']['mfmax']], [pars_dict['snow']['mfmin']],
                            [pars_dict['snow']['uadj']],[pars_dict['snow']['si']], [pars_dict['snow']['nmf']],
                            [pars_dict['snow']['tipm']],[pars_dict['snow']['mbase']], [pars_dict['snow']['plwhc']],
                            [pars_dict['snow']['daygm']],[pars_dict['snow']['adc_a']], [pars_dict['snow']['adc_b']],
                            [pars_dict['snow']['adc_c']]
                            ],axis=0)

        #Initate sacsnow_pars data class
        sacsnow_dc = nwsrfs.sacsnow_pars(year = self.year,month =self.month,day = self.day,hour = self.hour,
                                alat = pars_dict['snow']['alat'], elev = pars_dict['snow']['elev'], 
                                sac_pars =  sac_pars, snow_pars = snow_pars,
                                init_swe = pars_dict['snow']['init_swe'], 
                                pxadj =  pars_dict['sac']['pxadj'], peadj = pars_dict['sac']['peadj'],
                                forcings_map = self.forcings['map'].to_numpy(),
                                forcings_mat = self.forcings['mat'].to_numpy(),
                                forcings_ptps = self.forcings['ptps'].to_numpy(),
                                forcings_etd = self.forcings['etd'].to_numpy())

        #Initiate sacsnow wrapper class
        nwsrfs.sacsnow.__init__(self,
            pars_dataclass = sacsnow_dc,
            validate = validate)

    @property
    def sacsnow_tci(self) -> pd.DataFrame:
        '''
        Returns total channel inflow (tci) as a DataFrame with a column for each zone (units: mm).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.sacsnow_logic:
            return None

        #Get tci from sacsnow class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.sacsnow.sacsnow_tci.fget(self) 

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)    

    @property
    def sacsnow_states(self) -> dict[str, pd.DataFrame]:
        '''
        Returns a dictionary of DataFrames containing all model states with a column for each zone. 
            The dictionary keys are:

            * **tci**: Total channel inflow (units: mm).
            * **map_pxadj**: Precipitation after pxadj applied (units: mm).
            * **etd_peadj**: Evaporation demand after peadj, efc, and aesc adjustments applied (units: mm).
            * **aet**: Actual evapotranspiration (units: mm).
            * **uztwc**: Upper zone tension water contents (units: mm).
            * **uzfwc**: Upper zone free water contents (units: mm).
            * **lztwc**: Lower zone tension water contents (units: mm).
            * **lzfsc**: Lower zone free supplemental water contents (units: mm).
            * **lzfpc**: Lower zone free primary water contents (units: mm).
            * **adimc**: Additional impervious area water contents (units: mm).
            * **roimp**: Impervious runoff prior to riparian vegetation adjustment (units:  mm).
            * **sdro**: Direct runoff prior to riparian vegetation adjustment (units:  mm).
            * **ssur**: Surface runoff prior to riparian vegetation adjustment (units:  mm).
            * **sif**: Interflow prior to riparian vegetation adjustment (units:  mm).
            * **bfs**: Baseflow supplemental runoff prior to riparian vegetation adjustment (units:  mm).
            * **bfp**: Baseflow primary runoff prior to riparian vegetation adjustment (units:  mm).
            * **swe**: Snow water equivalent (units: mm).
            * **aesc**: Areal exent of snow cover (units: fraction 0-1).
            * **neghs**: Snowpack heat deficit (units:  mm).
            * **liqw**: Liquid water held by snow against gravity drainage (units: mm).
            * **raim**: Total rain plus snowmelt (units: mm).
            * **psfall**:  Precipitation falling as snow after scf adjustment has been applied (units: mm).
            * **prain**: Precipitation falling as rain (units: mm).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.sacsnow_logic:
            return None

        #Get states from sacsnow class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.sacsnow.sacsnow_states.fget(self)

        #For each dictionary value, rename column of Dataframe to correpond to zone names
        return {key: df.set_axis(self.zone_names, axis=1) for key, df in raw_output.items()}

    def return_uh(self,
                tstep):
        '''
        Returns a unit hydrograph as a DataFrame at a timestep specified by ``tstep``.
        Args:
            tstep (int): Specifies tstep of unit hydrograph to return (units: hours).
        Returns:
             pd.DataFrame: A DataFrame containing unit hydrograph (uh) with a column for each zone (units: cfs).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.sacsnow_logic:
            return None

        raw_output = nwsrfs.gamma_uh.return_uh(self,tstep)

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    @property
    def uh(self) -> pd.DataFrame:
        '''
        Returns a unit hydrograph at as a DataFrame at a timestep specified by the ``dt_hours`` attribute.
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.sacsnow_logic:
            return None

       #Get uh from gamma_uh class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.gamma_uh.uh.fget(self)

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    #create a new type for sacsnow_tci output
    SACSnowTCI = NewType('SACSnowTCI',pd.DataFrame)

    def return_sf(self,tci:SACSnowTCI,return_inst:bool = True):
        '''
        Return a timeseries of streamflow for each zone.
        Args:
            tci (SACSnowTCI): Specific tci DataFrame output from sacsnow classes sacsnow_tci property (units:  mm).
            return_inst (bool): The specifies to return instaneous streamflow, rather than period average.  Default: True
        Returns:
            pd.DataFrame: A DataFrame containing streamflow with a column for each zone (units: cfs).
        '''

        raw_output = nwsrfs.gamma_uh.return_sf(self,tci,return_inst)
        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)


    def sacsnow_sf(self,return_inst:bool=True,shift_sf:bool=True):
        '''
        Calculates streamflow for each zone using sacsnow and uh_gamma models. 

        Args:
            return_inst (bool):  Returns instantaneous streamflow simulation otherwise returns period average for each timestep. Default: True
            shift_sf (bool):  Shift streamflow forward on timestep.  Requirement for NWRFC calibrations. Default: True
        Returns:
            pd.DataFrame: Returns streamflow as a DataFrame with a column for each zone (units: cfs).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.sacsnow_logic:
            return None

        #Get tci from sacsnow class
        tci_output = self.sacsnow_tci

        #Get streamflow from return_sf gamma_uh class function
        sf_output = self.return_sf(tci=tci_output,return_inst=return_inst)

        #For NWRFC Calibrations, UH output needs to be shifted forward one timestep because of how forcings are treated in AutoCalb
        #Repeat the first flow data point and append to the beginning of the ts, so there is no loss in a timestep
        if shift_sf:
            shift = sf_output.shift(1)
            sf_output  = shift.combine_first(sf_output)

        return sf_output

    # def update_pars(self, pars):
    #     self.pars = pars

    # def lagk_run(self,n=None): 
        
    #     if self.n_upflow>0:
    #         if n is None:
    #             n=list(range(self.n_upflow))
    #         elif isinstance(n, int):
    #             n=[n]
                
    #         p = self.p['lagk']
                    
    #         lagk=s.lagk(int(self.dt_hours),int(self.dt_hours),
    #                     p['lagtbl_a'][n], p['lagtbl_b'][n], p['lagtbl_c'][n], p['lagtbl_d'][n],
    #                     p['ktbl_a'][n], p['ktbl_b'][n], p['ktbl_c'][n], p['ktbl_d'][n],
    #                     p['lagk_lagmax'][n], p['lagk_kmax'][n], p['lagk_qmax'][n],
    #                     p['lagk_lagmin'][n], p['lagk_kmin'][n], p['lagk_qmin'][n],
    #                     p['init_co'][n], p['init_if'][n], p['init_of'][n], p['init_stor'][n],
    #                     self.upflow[:,n],int(0))

    #         sim_flow_cfs = np.sum(lagk[0],axis=1)
            
    #         self.lagk_flow_cfs = pd.Series(sim_flow_cfs, index=self.dates)
            
    #         return self.lagk_flow_cfs
    #     else:
    #         return np.nan   
    # def lagk_states_run(self): 
        
    #     if self.n_upflow>0:
    #         #if n is None:
    #         #    n=list(range(self.n_upflow))
    #         #elif isinstance(n, int):
    #         #    n=[n]
    #         #    
    #         p = self.p['lagk']
                    
    #         states=s.lagk(int(self.dt_hours),int(self.dt_hours),
    #                     p['lagtbl_a'], p['lagtbl_b'], p['lagtbl_c'], p['lagtbl_d'],
    #                     p['ktbl_a'], p['ktbl_b'], p['ktbl_c'], p['ktbl_d'],
    #                     p['lagk_lagmax'], p['lagk_kmax'], p['lagk_qmax'],
    #                     p['lagk_lagmin'], p['lagk_kmin'], p['lagk_qmin'],
    #                     p['init_co'], p['init_if'], p['init_of'], p['init_stor'],
    #                     self.upflow,int(1))

    #         state_param=['routed','lag_time','k_inflow','k_storage']
            
    #         self.lagk_states={}
    #         for count, param in  enumerate(state_param):
    #             self.lagk_states[param]=pd.DataFrame(states[count], index=self.dates,columns=self.upflow_name)
            
    #         return self.lagk_states
    #     else:
    #         return np.nan


    # def sacsnow_run(self,inst=True):

    #         if self.n_zones>0:
    #             p = {**self.p['sac'],**(self.p['snow']),**(self.p['uh'])}
     

    #             states = s.sacsnow(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'), self.hour.astype('int'),
    #                             # general pars
    #                             p['alat'].astype('double'), p['elev'].astype('double'),
    #                             # sac pars
    #                             self.sac_pars,
    #                             # pet and precp adjustments
    #                             p['peadj'].astype('double'), p['pxadj'].astype('double'),
    #                             # snow pars
    #                             self.snow_pars,
    #                             # initial swe
    #                             p['init_swe'].astype('double'),
    #                             # forcings
    #                             self.forcings.map_fa, self.forcings.ptps_fa, self.forcings.mat_fa,self.forcings.etd,
    #                             #Pass states option
    #                             int(0))
                
    #             tci=states[2]
                
    #             # channel routing
    #             self.sacsnow_flow_cfs = self.uh.tci_2_cfs(tci,self.dates,inst=inst)

    #             #Recalculate FA forcing due to Map and ETD being modified
    #             self.forcings.fa_ts(self.dt_seconds,self.dates)

    #             return self.sacsnow_flow_cfs
    #         else:
                # return np.nan

#     def sacsnow_states_run(self,inst=True):

#         if self.n_zones>0: 

#             p = {**self.p['sac'],**(self.p['snow']),**(self.p['uh'])}

#             # simulates all zones
#             states = s.sacsnow(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'), self.hour.astype('int'),
#                             # general pars
#                             p['alat'].astype('double'), p['elev'].astype('double'),
#                             # sac pars
#                             self.sac_pars,
#                             # pet and precp adjustments
#                             p['peadj'].astype('double'), p['pxadj'].astype('double'),
#                             # snow pars
#                             self.snow_pars,
#                             # initial swe
#                             p['init_swe'].astype('double'),
#                             # forcings
#                             self.forcings.map_fa, self.forcings.ptps_fa, self.forcings.mat_fa,self.forcings.etd,
#                             #Pass states option
#                             int(1))

#             state_param=['map_pxadj','etd_adj','tci','aet',
#                             'uztwc','uzfwc','lztwc','lzfsc','lzfpc','adimc',
#                             'roimp', 'sdro', 'ssur', 'sif', 'bfs', 'bfp',
#                             'swe','aesc','neghs','liqw','raim','psfall','prain']
#             self.sacsnow_states={}
#             for count, param in  enumerate(state_param):
#                 self.sacsnow_states[param]=pd.DataFrame(states[count], index=self.dates,columns=self.zones)

#             #Calculate streamflow for each zone
#             sf_df=pd.DataFrame()
#             for count, zone in enumerate(self.zones):
#                 tci_zone=self.sacsnow_states['tci'][zone].astype('double').to_numpy()
#                 tci_zone=np.expand_dims(tci_zone,axis=1)
#                 tci_zone=np.asfortranarray(tci_zone)
#                 sf_zones=self.uh.tci_2_cfs(tci_zone,self.dates,count,inst=inst).rename(zone)
#                 sf_df=pd.concat([sf_df,sf_zones],axis=1,ignore_index=True)
#             sf_df.index=self.dates
#             sf_df.columns=self.zones
            
#             self.sacsnow_states['sf']=sf_df
            
#             #Recalculate FA forcing due to Map and ETD being modified
#             self.forcings.fa_ts(self.dt_seconds,self.dates)
            
#             return self.sacsnow_states
#         else:
#             return np.nan
    
#     def consuse_run(self):

#         if self.n_consuse>0:
#             p = self.p['consuse']
#             cms_2_cfs=35.3147
            
#             #Get natural flow
#             #Create blank simulation series
#             qnat=pd.Series(0,index=self.dates)
            
#             #If there are sac/snow zone, calculate runoff
#             if self.n_zones > 0:
#                 qnat = qnat+self.sacsnow_run(inst=True)
            
#             #If there are upstream reaches to route, add them to the total flow
#             if self.n_upflow > 0:
#                 qnat = self.lagk_run() + qnat

#             #Chanloss Adjustment
#             qnat=self.chanloss(qnat)

#             #Convert to daily using the weighting scheme that CHPS uses of utilizing 5 points (edges assigned .5)
#             qnat_daily=(qnat.rolling(5,center=True).sum()+qnat.rolling(3,center=True).sum())/8
#             qnat_daily=qnat_daily.loc[qnat_daily.index.hour==12]
#             qnat_daily=qnat_daily.resample('1D').sum()
            
#             #Get PET
#             pet=pd.DataFrame(self.forcings.pet,columns=self.zones,index=self.dates)
            
#             #Create a blank state dataframe
#             state_param=['QADJ','QDIV','QRF_in','QRF_out','QOL','QCD','CE','RFSTOR']
#             self.consuse_states={}
#             for count, param in  enumerate(state_param):
#                 self.consuse_states[param]=pd.DataFrame()
            
#             #Run consuse for each zone individualys
#             for n, cu_name in zip(range(self.n_consuse), self.consuse_name):
                
#                 #Get PET from equivalent SAC zone.  
#                 #NOTE:  To match CHPS results have to be shifted back 1 hr so 00:00 timestep
#                 #       is included in previous day
#                 pet_daily=pet[cu_name].shift(periods=-1, freq='hours').resample('1D').sum()

#                 consuse_ts_input=pd.concat([pet_daily,qnat_daily],axis=1)
#                 consuse_ts_input.columns=['pet','qnat']
#                 consuse_ts_input=consuse_ts_input[~consuse_ts_input.isna().any(axis=1)]\
                
#                 peadj=self.pars.loc[(self.pars.name=='peadj')&(self.pars.zone==cu_name),'value'].squeeze()
                
#                 dates_input=consuse_ts_input.index
                
#                 consuse_ts_input=consuse_ts_input.astype('double').to_numpy()
#                 consuse_ts_input=np.asfortranarray(consuse_ts_input)
                
#                 #pet_iput=consuse_ts_input.pet.astype('double').to_numpy()
#                 #pet_input=np.asfortranarray(pet_input)
                
#                 #qnat_iput=consuse_ts_input.qnat.astype('double').to_numpy()
#                 #qnat_input=np.asfortranarray(qnat_input)
                
#                 states=s.consuse(dates_input.year.astype('int'), dates_input.month.astype('int'), dates_input.day.astype('int'),
#                              p['area_km2'][n].astype('double'),p['irr_eff'][n].astype('double'),np.double(p['min_flow_cmsd'][n]*cms_2_cfs),
#                              p['rf_accum_rate'][n].astype('double'),p['rf_decay_rate'][n].astype('double'),
#                              self.peadj_cu[:,n],peadj,
#                              consuse_ts_input[:,0],consuse_ts_input[:,1])
                
#                 #Concat state value for CU zone to dictionary. IF QADJ 
#                 for count, param in  enumerate(state_param):
#                     if param=='QADJ':
#                         self.consuse_states[param]=pd.DataFrame(states[count], index=dates_input,columns=[param])
#                     else:
#                         self.consuse_states[param]=pd.concat([self.consuse_states[param],
#                                 pd.DataFrame(states[count], index=dates_input,columns=[cu_name])],axis=1)
#                 #Update the qnat to reflect the adjusted flow (needed for basins w/multiple CU zones)
#                 qnat_daily=self.consuse_states['QADJ']
            
#             return self.consuse_states
#         else:
#             return np.nan

#     def chanloss(self,sim_sf):
    
#         #Check if there is a chanloss module
#         if self.n_chanloss==0:
#             sim_sf_adj=sim_sf
#         else:
#             p = self.p['chanloss']
            
#             periods =  np.column_stack((p['cl_period_start'],p['cl_period_end'])).astype(int)
           
#             sim_sf_adj=s.chanloss(int(self.dt_seconds), self.year.astype('int'), self.month.astype('int'), self.day.astype('int'),
#                         p['cl_factor'],periods,p['cl_type'].astype('int'), p['cl_min_q'].astype('double'),
#                         sim_sf.astype('double').to_numpy())
#             sim_sf_adj=pd.Series(sim_sf_adj, index=self.dates)
#         return sim_sf_adj

#     def run_all(self,inst=True):
        
#         #Create blank simulation series
#         self.sim=pd.Series(0,index=self.dates)
        
#         #If there are sac/snow zone, calculate runoff
#         if self.n_zones > 0:
#             self.sim = self.sim+self.sacsnow_run(inst=True)
        
#         #If there are upstream reaches to route, add them to the total flow
#         if self.n_upflow > 0:
#             self.sim = self.lagk_run() + self.sim

#         #Chanloss Adjustment
#         self.sim=self.chanloss(self.sim)

#         #If there area CONSUSE areas, adjust the flow
#         if self.n_consuse > 0:
#             self.consuse_run()
#             qnat_cu_adj=self.consuse_states['QDIV'].sum(axis=1)-self.consuse_states['QRF_out'].sum(axis=1)
#             #Shift forward a day so that the correct adjustement is applied  to the correct day
#             qnat_cu_adj.index=qnat_cu_adj.index+pd.Timedelta(1, unit='D')
#             #Backfill to fill all values after 00:00 and forward fill to correct missing values at end of timeseries
#             qnat_cu_adj=qnat_cu_adj.reindex(self.sim.index).backfill().ffill()
#             q_adj=self.sim - qnat_cu_adj
#             self.sim = self.sim - qnat_cu_adj
#             #Replace any negative values with zero
#             self.sim[self.sim < 0]=0

#         #return instantaneous or period avg depending on chosen option
#         if not inst:
#             current_sim=self.sim.to_numpy().flatten()
#             next_sim = pd.DataFrame(self.sim).shift(-1).to_numpy().flatten()
#             sim_flow_pavg_cfs = (current_sim + next_sim) / 2
#             self.sim = pd.Series(sim_flow_pavg_cfs, index=self.dates)

#         return self.sim